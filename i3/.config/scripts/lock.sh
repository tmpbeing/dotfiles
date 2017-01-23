#!/bin/bash

TEMPDIR=/tmp

sh $HOME/.config/scripts/individual_scrot.sh
convert $TEMPDIR/head_0.png -scale 10% -scale 1000% $TEMPDIR/head_0.png
[[ -f /home/snoop/.config/i3/rmlock.png ]] && convert $TEMPDIR/head_0.png /home/snoop/.config/i3/rmlock.png -gravity center -composite -matte $TEMPDIR/head_0.png
if [ -e $TEMPDIR/head_1.png ]; then
  convert $TEMPDIR/head_1.png -scale 10% -scale 1000% $TEMPDIR/head_1.png
  [[ -f /home/snoop/.config/i3/rmlock.png ]] && convert $TEMPDIR/head_1.png /home/snoop/.config/i3/rmlock.png -gravity center -composite -matte $TEMPDIR/head_1.png
  convert $TEMPDIR/head_0.png $TEMPDIR/head_1.png +append $TEMPDIR/screen.png
else
  mv $TEMPDIR/head_0.png $TEMPDIR/screen.png
fi
i3lock -u -i $TEMPDIR/screen.png
