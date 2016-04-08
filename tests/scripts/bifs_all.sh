
#@bt_test execute tests on BT file: BT<->XMT, BT<->MP4, XMT<->MP4,  conversions BT, XMT and MP4 Playback
bt_test ()
{
 btfile=$1
 xmtfile=${btfile%.*}'.xmt'
 mp4file=${btfile%.*}'.mp4'
 libfile=""
 name=$(basename $1)
 name=${name%.*}

 #file used by other test
 if [[ $btfile = *"-inline.bt" ]] ; then
  return
 fi

 #start our test, specifying all hash names we will check
 test_begin "$name" "bt-to-xmt" "xmt-to-bt" "xmt-to-mp4" "bt-to-mp4" "mp4-to-bt" "mp4-to-xmt" "play"
 if [ $test_skip  = 1 ] ; then
  return
 fi

 #check for extern proto, and make MP4 out of lib
 if [[ $btfile == *"externproto"* ]] ; then
  if [[ $btfile != *"-lib.bt" ]] ; then
   libfile="${btfile%.*}-lib.bt"
   do_test "$MP4BOX -mp4 $libfile" "Proto lib BT->MP4"
   libfile="${btfile%.*}-lib.mp4"
  fi
 fi

 #check for *-inline used in linking and mediacontrol - the dependent file is $basename-inline.bt
 if [[ $btfile == *"-inline"* ]] ; then
  if [[ $btfile != *"inline-http" ]] ; then
   libfile="${btfile%.*}-inline.bt"
   if [ -f $libfile ] ; then
	do_test "$MP4BOX -mp4 $libfile" "Inline BT->MP4"
	libfile="${btfile%.*}-inline.mp4"
   else
	libfile=""
   fi
  fi
 fi

 #first do BT->MP4
 do_test "$MP4BOX -mp4 $btfile" "BT2MP4" && do_hash_test "$mp4file" "bt-to-mp4"

 #then BT->XMT
 do_test "$MP4BOX -xmt $btfile" "BT2XMT" && do_hash_test "$xmtfile" "bt-to-xmt"

 #then all following tests can be run in parallel

 #XMT->BT
 do_test "$MP4BOX -bt $xmtfile -out test1.bt" "XMT2BT" && do_hash_test "test1.bt" "xmt-to-bt" && rm test1.bt 2> /dev/null &

 #XMT->MP4
 do_test "$MP4BOX -mp4 $xmtfile -out $mp4file.tmp" "XMT2MP4" && do_hash_test "$mp4file.tmp" "xmt-to-mp4" && rm "$mp4file.tmp" &

 #MP4->BT
 do_test "$MP4BOX -bt $mp4file -out test2.bt" "MP42BT" && do_hash_test "test2.bt" "mp4-to-bt" && rm test2.bt 2> /dev/null &

 #MP4->XMT
 do_test "$MP4BOX -xmt $mp4file -out test.xmt" "MP42XMT" && do_hash_test "test.xmt" "mp4-to-xmt" && rm test.xmt 2> /dev/null &

 if [ $play_all = 1 ] ; then

  #BT playback
  do_test "$MP4CLIENT -run-for 1 $btfile" "bt-play" &

  #XMT playback
  do_test "$MP4CLIENT -run-for 1 $xmtfile" "xmt-play" &

 fi

 #MP4 playback - dump 10 sec of AVI and hash it. This should be enough for most of our sequences ...
 do_playback_test $mp4file "play" &

 #this will sync everything, we can delete after
 test_end

 rm $xmtfile
 rm $mp4file

 if [ "$libfile" != "" ] ; then
  rm $libfile
 fi

}


bifs_tests ()
{

 for bt in $MEDIA_DIR/bifs/*.bt ; do
  bt_test $bt
 done
}

bifs_tests

#bt_test "bifs/bifs-2D-background-background2D-bind.bt"
#bt_test "bifs/bifs-game-bubble.bt"
#bt_test "bifs-media-movietexture-no-od.bt"

