/*
 *			GPAC - Multimedia Framework C SDK
 *
 *			Authors: Jean Le Feuvre, Cyril Concolato, Romain Bouqueau
 *			Copyright (c) Telecom ParisTech 2005-2012
 *					All rights reserved
 *
 *  This file is part of GPAC / mp4-to-ts (mp42ts) application
 *
 *  GPAC is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  GPAC is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; see the file COPYING.  If not, write to
 *  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#include <gpac/media_tools.h>
#include <gpac/constants.h>
#include <gpac/base_coding.h>
#include <gpac/mpegts.h>

#ifdef GPAC_DISABLE_MPEG2TS_MUX
#error "Cannot compile MP42TS if GPAC is not built with MPEG2-TS Muxing support"
#endif

#ifdef GPAC_DISABLE_ISOM
#error "Cannot compile MP42TS if GPAC is not built with ISO File Format support"
#endif

static GFINLINE void usage()
{
	fprintf(stderr, "GPAC version " GPAC_FULL_VERSION "\n"
	        "GPAC Copyright (c) Telecom ParisTech 2000-2014\n"
	        "GPAC Configuration: " GPAC_CONFIGURATION "\n"
	        "Features: %s\n\n", gpac_features());
	fprintf(stderr, "mp42ts <inputs> <destinations> [options]\n"
	        "\n"
	        "Inputs:\n"
	        "-src filename[:OPTS]   specifies an input file used for a TS service\n"
	        "                        * currently only supports ISO files and SDP files\n"
	        "                        * can be used several times, once for each program\n"
	        "By default each source is a program in a TS. \n"

	        "\n"
	        "-prog filename        same as -src filename\n"
	        "\n"
	        "Destinations:\n"
	        "Several destinations may be specified as follows, at least one is mandatory\n"
	        "-dst-file filename\n"
	        "\n"
	        "Misc options\n"
	        "-h or -help            print this screen\n"
	        "\n"
	       );
}


typedef struct
{
	GF_ISOFile *mp4;
	u32 nb_streams, pcr_idx;
	GF_ESInterface streams[40];
	u64 samples_done, samples_count;
	u32 nb_real_streams;
} M2TSSource;

typedef struct
{
	GF_ISOFile *mp4;
	u32 track, sample_number, sample_count;
	GF_ISOSample *sample;

	s64 ts_offset, cts_dts_shift;
	M2TSSource *source;
} GF_ESIMP4;

static GF_Err mp4_input_ctrl(GF_ESInterface *ifce, u32 act_type, void *param)
{
	GF_ESIMP4 *priv = (GF_ESIMP4 *)ifce->input_udta;
	if (!priv) return GF_BAD_PARAM;

	switch (act_type) {
	case GF_ESI_INPUT_DATA_FLUSH:
	{
		GF_ESIPacket pck;
		if (!priv->sample)
			priv->sample = gf_isom_get_sample(priv->mp4, priv->track, priv->sample_number+1, NULL);

		if (!priv->sample) {
			return GF_IO_ERR;
		}

		memset(&pck, 0, sizeof(GF_ESIPacket));

		pck.flags = GF_ESI_DATA_AU_START | GF_ESI_DATA_HAS_CTS;
		if (priv->sample->IsRAP) pck.flags |= GF_ESI_DATA_AU_RAP;
		pck.cts = priv->sample->DTS + priv->ts_offset;

		pck.dts = pck.cts;
		if (priv->cts_dts_shift) {
			pck.cts += + priv->cts_dts_shift;
			pck.flags |= GF_ESI_DATA_HAS_DTS;
		}
		
		if (priv->sample->CTS_Offset) {
			pck.cts += priv->sample->CTS_Offset;
			pck.flags |= GF_ESI_DATA_HAS_DTS;
		}

		pck.flags |= GF_ESI_DATA_AU_END;
		pck.data = priv->sample->data;
		pck.data_len = priv->sample->dataLength;
		pck.duration = gf_isom_get_sample_duration(priv->mp4, priv->track, priv->sample_number+1);
		ifce->output_ctrl(ifce, GF_ESI_OUTPUT_DATA_DISPATCH, &pck);

		gf_isom_sample_del(&priv->sample);
		priv->sample_number++;

		priv->source->samples_done++;
		gf_set_progress("Converting to MPEG-2 TS", priv->source->samples_done, priv->source->samples_count);

		if (priv->sample_number==priv->sample_count) {
			if (!(ifce->caps & GF_ESI_STREAM_IS_OVER)) {
				ifce->caps |= GF_ESI_STREAM_IS_OVER;
				if (priv->sample_count>1) {
					assert(priv->source->nb_real_streams);
					priv->source->nb_real_streams--;
				}
			}
		}
	}
	return GF_OK;

	case GF_ESI_INPUT_DESTROY:
		if (ifce->decoder_config) {
			gf_free(ifce->decoder_config);
			ifce->decoder_config = NULL;
		}
		gf_free(priv);
		ifce->input_udta = NULL;
		return GF_OK;
	default:
		return GF_BAD_PARAM;
	}
}

static void fill_isom_es_ifce(M2TSSource *source, GF_ESInterface *ifce, GF_ISOFile *mp4, u32 track_num)
{
	GF_ESIMP4 *priv;
	char *_lan;
	GF_ESD *esd;
	u64 avg_rate, duration;
	s32 ref_count;
	s64 mediaOffset;

	GF_SAFEALLOC(priv, GF_ESIMP4);
	if (!priv) {
		return;
	}

	priv->mp4 = mp4;
	priv->track = track_num;
	priv->sample_count = gf_isom_get_sample_count(mp4, track_num);
	source->samples_count += priv->sample_count;
	if (priv->sample_count>1)
		source->nb_real_streams++;

	priv->source = source;
	memset(ifce, 0, sizeof(GF_ESInterface));
	ifce->stream_id = gf_isom_get_track_id(mp4, track_num);

	esd = gf_media_map_esd(mp4, track_num);

	if (esd) {
		ifce->stream_type = esd->decoderConfig->streamType;
		ifce->object_type_indication = esd->decoderConfig->objectTypeIndication;
		if (esd->decoderConfig->decoderSpecificInfo && esd->decoderConfig->decoderSpecificInfo->dataLength) {
			switch (esd->decoderConfig->objectTypeIndication) {
			case GPAC_OTI_AUDIO_AAC_MPEG4:
			case GPAC_OTI_AUDIO_AAC_MPEG2_MP:
			case GPAC_OTI_AUDIO_AAC_MPEG2_LCP:
			case GPAC_OTI_AUDIO_AAC_MPEG2_SSRP:
			case GPAC_OTI_VIDEO_MPEG4_PART2:
				ifce->decoder_config = (char *)gf_malloc(sizeof(char)*esd->decoderConfig->decoderSpecificInfo->dataLength);
				ifce->decoder_config_size = esd->decoderConfig->decoderSpecificInfo->dataLength;
				memcpy(ifce->decoder_config, esd->decoderConfig->decoderSpecificInfo->data, esd->decoderConfig->decoderSpecificInfo->dataLength);
				break;
			case GPAC_OTI_VIDEO_AVC:
			case GPAC_OTI_VIDEO_SVC:
			case GPAC_OTI_VIDEO_MVC:
				gf_isom_set_nalu_extract_mode(mp4, track_num, GF_ISOM_NALU_EXTRACT_LAYER_ONLY | GF_ISOM_NALU_EXTRACT_INBAND_PS_FLAG | GF_ISOM_NALU_EXTRACT_ANNEXB_FLAG | GF_ISOM_NALU_EXTRACT_VDRD_FLAG);
				break;
			case GPAC_OTI_SCENE_VTT_MP4:
				ifce->decoder_config = (char *)gf_malloc(sizeof(char)*esd->decoderConfig->decoderSpecificInfo->dataLength);
				ifce->decoder_config_size = esd->decoderConfig->decoderSpecificInfo->dataLength;
				memcpy(ifce->decoder_config, esd->decoderConfig->decoderSpecificInfo->data, esd->decoderConfig->decoderSpecificInfo->dataLength);
				break;
			}
		}
		gf_odf_desc_del((GF_Descriptor *)esd);
	}
	gf_isom_get_media_language(mp4, track_num, &_lan);
	if (!_lan || !strcmp(_lan, "und")) {
		ifce->lang = 0;
	} else {
		ifce->lang = GF_4CC(_lan[0],_lan[1],_lan[2],' ');
	}
	if (_lan) {
		gf_free(_lan);
	}

	ifce->timescale = gf_isom_get_media_timescale(mp4, track_num);
	ifce->duration = gf_isom_get_media_timescale(mp4, track_num);
	avg_rate = gf_isom_get_media_data_size(mp4, track_num);
	avg_rate *= ifce->timescale * 8;
	if (0!=(duration=gf_isom_get_media_duration(mp4, track_num)))
		avg_rate /= duration;

	if (gf_isom_has_time_offset(mp4, track_num)) ifce->caps |= GF_ESI_SIGNAL_DTS;

	ifce->bit_rate = (u32) avg_rate;
	ifce->duration = (Double) (s64) gf_isom_get_media_duration(mp4, track_num);
	ifce->duration /= ifce->timescale;

	GF_SAFEALLOC(ifce->sl_config, GF_SLConfig);
	if (!ifce->sl_config) {
		return;
	}
	
	ifce->sl_config->tag = GF_ODF_SLC_TAG;
	ifce->sl_config->useAccessUnitStartFlag = 1;
	ifce->sl_config->useAccessUnitEndFlag = 1;
	ifce->sl_config->useRandomAccessPointFlag = 1;
	ifce->sl_config->useTimestampsFlag = 1;
	ifce->sl_config->timestampLength = 33;
	ifce->sl_config->timestampResolution = ifce->timescale;

#ifdef GPAC_DISABLE_ISOM_WRITE
	fprintf(stderr, "Warning: GPAC was compiled without ISOM Write support, can't set SL Config!\n");
#else
	gf_isom_set_extraction_slc(mp4, track_num, 1, ifce->sl_config);
#endif

	ifce->input_ctrl = mp4_input_ctrl;
	if (priv != ifce->input_udta) {
		if (ifce->input_udta)
			gf_free(ifce->input_udta);
		ifce->input_udta = priv;
	}


	if (! gf_isom_get_edit_list_type(mp4, track_num, &mediaOffset)) {
		priv->ts_offset = mediaOffset;
	}

	if (gf_isom_has_time_offset(mp4, track_num)==2) {
		priv->cts_dts_shift = gf_isom_get_cts_to_dts_shift(mp4, track_num);
	}

	ifce->depends_on_stream = 0;
	ref_count = gf_isom_get_reference_count(mp4, track_num, GF_ISOM_REF_SCAL);
	if (ref_count > 0) {
		gf_isom_get_reference_ID(mp4, track_num, GF_ISOM_REF_SCAL, (u32) ref_count, &ifce->depends_on_stream);
	}
}

static volatile Bool run = 1;

static Bool open_source(M2TSSource *source, char *src)
{
	memset(source, 0, sizeof(M2TSSource));

	/*open ISO file*/
	if (gf_isom_probe_file(src)) {
		u32 i;
		u32 nb_tracks;
		u32 first_audio = 0;
		u32 first_other = 0;
		s64 min_offset = 0;
		u32 min_offset_timescale = 0;
		source->mp4 = gf_isom_open(src, GF_ISOM_OPEN_READ, 0);
		source->nb_streams = 0;
		/*on MPEG-2 TS, carry 3GPP timed text as MPEG-4 Part17*/
		gf_isom_text_set_streaming_mode(source->mp4, 1);
		nb_tracks = gf_isom_get_track_count(source->mp4);

		for (i=0; i<nb_tracks; i++) {
			Bool check_deps = 0;
			if (gf_isom_get_media_type(source->mp4, i+1) == GF_ISOM_MEDIA_HINT)
				continue;

			fill_isom_es_ifce(source, &source->streams[i], source->mp4, i+1);
			if (min_offset > ((GF_ESIMP4 *)source->streams[i].input_udta)->ts_offset) {
				min_offset = ((GF_ESIMP4 *)source->streams[i].input_udta)->ts_offset;
				min_offset_timescale = source->streams[i].timescale;
			}

			switch(source->streams[i].stream_type) {
			case GF_STREAM_VISUAL:
				check_deps = 1;
				if (gf_isom_get_sample_count(source->mp4, i+1)>1) {
					/*get first visual stream as PCR*/
					if (!source->pcr_idx) {
						source->pcr_idx = i+1;
					}
				}
				break;
			case GF_STREAM_AUDIO:
				if (!first_audio) first_audio = i+1;
				check_deps = 1;
				break;
			default:
				/*log not supported stream type: %s*/
				break;
			}
			source->nb_streams++;
			if (gf_isom_get_sample_count(source->mp4, i+1)>1) first_other = i+1;

			if (check_deps) {
				u32 k;
				Bool found_dep = 0;
				for (k=0; k<nb_tracks; k++) {
					if (gf_isom_get_media_type(source->mp4, k+1) != GF_ISOM_MEDIA_OD)
						continue;

					/*this stream is not refered to by any OD, send as regular PES*/
					if (gf_isom_has_track_reference(source->mp4, k+1, GF_ISOM_REF_OD, gf_isom_get_track_id(source->mp4, i+1) )==1) {
						found_dep = 1;
						break;
					}
				}
				if (!found_dep) {
					source->streams[i].caps |= GF_ESI_STREAM_WITHOUT_MPEG4_SYSTEMS;
				}
			}
		}

		/*if no visual PCR found, use first audio*/
		if (!source->pcr_idx) source->pcr_idx = first_audio;
		if (!source->pcr_idx) source->pcr_idx = first_other;
		if (source->pcr_idx) {
			GF_ESIMP4 *priv;
			source->pcr_idx-=1;
			priv = source->streams[source->pcr_idx].input_udta;
			gf_isom_set_default_sync_track(source->mp4, priv->track);
		}

		if (min_offset < 0) {
			for (i=0; i<source->nb_streams; i++) {
				Double scale = source->streams[i].timescale;
				scale /= min_offset_timescale;
				((GF_ESIMP4 *)source->streams[i].input_udta)->ts_offset += (s64) (-min_offset * scale);
			}
		}

		return 1;
	}

	{
		FILE *f = gf_fopen(src, "rt");
		if (f) {
			gf_fclose(f);
			fprintf(stderr, "Error opening %s - not a supported input media, skipping.\n", src);
		} else {
			fprintf(stderr, "Error opening %s - no such file.\n", src);
		}
		return 0;
	}
}

/*macro to keep retro compatibility with '=' and spaces in parse_args*/
#define CHECK_PARAM(param) (!strnicmp(arg, param, strlen(param)) \
        && (   ((arg[strlen(param)] == '=') && (next_arg = arg+strlen(param)+1)) \
            || ((strlen(arg) == strlen(param)) && ++i && (i<argc) && (next_arg = argv[i]))))

/*parse MP42TS arguments*/
static GFINLINE GF_Err parse_args(int argc, char **argv, char **mp4_in, char **ts_out)
{
	Bool input_found=0, dst_found=0;
	char *arg = NULL, *next_arg = NULL, *error_msg = "no argument found";
	s32 i;

	/*first pass: find audio - NO GPAC INIT MODIFICATION MUST OCCUR IN THIS PASS*/
	for (i=1; i<argc; i++) {
		arg = argv[i];
		if (!stricmp(arg, "-h") || strstr(arg, "-help")) {
			usage();
			return GF_EOS;
		} else if (CHECK_PARAM("-dst-file")) {
			dst_found = 1;
			*ts_out = gf_strdup(next_arg);
		} else if (CHECK_PARAM("-src")) { //second pass arguments
		} else if (CHECK_PARAM("-prog")) { //second pass arguments
		}
		else {
			error_msg = "unknown option";
			goto error;
		}
	}
	if (!dst_found) {
		fprintf(stderr, "Error: Destination argument not found\n");
		usage();
		return GF_BAD_PARAM;
	}

	/*second pass: open sources*/
	for (i=1; i<argc; i++) {
		arg = argv[i];
		if (arg[0] !='-') continue;

		if (! CHECK_PARAM("-src") && ! CHECK_PARAM("-prog") ) continue;

		input_found = 1;
		*mp4_in = gf_strdup(next_arg);
		return GF_OK;
	}
	if (!input_found) {
		fprintf(stderr, "Error: Source argument not found\n");
		usage();
		return GF_BAD_PARAM;
	}

error:
	if (!arg) {
		fprintf(stderr, "Error: %s\n", error_msg);
	} else {
		fprintf(stderr, "Error: %s \"%s\"\n", error_msg, arg);
	}
	return GF_BAD_PARAM;
}

int main(int argc, char **argv)
{
	/********************/
	/*   declarations   */
	/********************/
	const char *ts_pck;
	u32 usec_till_next;
	u32 j, cur_pid, psi_refresh_rate, pcr_ms;
	char *mp4_in = NULL;
	char *ts_out = NULL;
	FILE *ts_output_file = NULL;
	M2TSSource source;
	GF_M2TS_Mux *muxer;

	/***********************/
	/*   initialisations   */
	/***********************/
	mp4_in = NULL;
	ts_output_file = NULL;
	ts_out = NULL;
	pcr_ms = 100;
	muxer = NULL;
	psi_refresh_rate = GF_M2TS_PSI_DEFAULT_REFRESH_RATE;

	/***********************/
	/*   parse arguments   */
	/***********************/
	if (GF_OK != parse_args(argc, argv, &mp4_in, &ts_out)) {
		goto exit;
	}

	if (!open_source(&source, mp4_in)) {
		goto exit;
	}

	/***************************/
	/*   create mp42ts muxer   */
	/***************************/
	muxer = gf_m2ts_mux_new(0, psi_refresh_rate, 0);
	if (!muxer) {
		fprintf(stderr, "Could not create the muxer. Aborting.\n");
		goto exit;
	}
	gf_m2ts_mux_use_single_au_pes_mode(muxer, GF_M2TS_PACK_AUDIO_ONLY);
	gf_m2ts_mux_set_pcr_max_interval(muxer, pcr_ms);

	ts_output_file = gf_fopen(ts_out, "wb");
	if (!ts_output_file) {
		fprintf(stderr, "Error opening %s\n", ts_out);
		goto exit;
	}

	/****************************************/
	/*   declare all streams to the muxer   */
	/****************************************/
	cur_pid = 100;	/*PIDs start from 100*/

	GF_M2TS_Mux_Program *program;

	u32 prog_pcr_offset = 0;
	fprintf(stderr, "Setting up program ID 1 - send rates: PSI %d ms PCR %d ms - PCR offset %d\n", psi_refresh_rate, pcr_ms, prog_pcr_offset);

	program = gf_m2ts_mux_program_add(muxer, 1, cur_pid, psi_refresh_rate, prog_pcr_offset, GF_M2TS_MPEG4_SIGNALING_NONE);
	if (program) {
		for (j=0; j<source.nb_streams; j++) {
			GF_M2TS_Mux_Stream *stream;
			Bool force_pes_mode = 0;
			/*likely an OD stream disabled*/
			if (!source.streams[j].stream_type) continue;

			stream = gf_m2ts_program_stream_add(program, &source.streams[j], cur_pid+j+1, (source.pcr_idx==j) ? 1 : 0, force_pes_mode);
		}

		cur_pid += source.nb_streams;
		while (cur_pid % 10) {
			cur_pid ++;
		}
	}

	muxer->flush_pes_at_rap = GF_FALSE;

	gf_m2ts_mux_update_config(muxer, 1);

	/*****************/
	/*   main loop   */
	/*****************/
	while (run) {
		u32 status;

		/*flush all packets*/
		while ((ts_pck = gf_m2ts_mux_process(muxer, &status, &usec_till_next)) != NULL) {
			if (ts_output_file != NULL) {
				gf_fwrite(ts_pck, 1, 188, ts_output_file);
			}

			if (status>=GF_M2TS_STATE_PADDING) {
				break;
			}
		}

		if (status==GF_M2TS_STATE_EOS) {
			break;
		}
	}

	{
		u64 bits = muxer->tot_pck_sent*8*188;
		u64 dur_ms = gf_m2ts_get_ts_clock(muxer);
		if (!dur_ms) dur_ms = 1;
		fprintf(stderr, "Done muxing - %.02f sec - average bitrate %d kbps "LLD" packets written\n", ((Double) dur_ms)/1000.0, (u32) (bits/dur_ms), muxer->tot_pck_sent);
		fprintf(stderr, " Padding: "LLD" packets (%g kbps) - "LLD" PES padded bytes (%g kbps)\n", muxer->tot_pad_sent, (Double) (muxer->tot_pad_sent*188*8.0/dur_ms) , muxer->tot_pes_pad_bytes, (Double) (muxer->tot_pes_pad_bytes*8.0/dur_ms) );
	}

exit:
	run = 0;
	if (mp4_in) gf_free(mp4_in);
	if (ts_output_file) gf_fclose(ts_output_file);
	if (ts_out) gf_free(ts_out);
	if (muxer) gf_m2ts_mux_del(muxer);

	for (j=0; j<source.nb_streams; j++) {
		if (source.streams[j].input_ctrl) {
			source.streams[j].input_ctrl(&source.streams[j], GF_ESI_INPUT_DESTROY, NULL);
		}
		if (source.streams[j].input_udta) {
			gf_free(source.streams[j].input_udta);
		}
		if (source.streams[j].decoder_config) {
			gf_free(source.streams[j].decoder_config);
		}
		if (source.streams[j].sl_config) {
			gf_free(source.streams[j].sl_config);
		}
	}
	if (source.mp4) {
		gf_isom_close(source.mp4);
	}

	return 0;
}

