CFLAGS = -g -O2

all: pixmaps.o

pixmaps.o: pixmaps.c \
  arrow_xpm box_xpm breakat_xpm cancel_xpm cont_xpm display_xpm \
  display_small_xpm dot_xpm break_xpm \
  down_xpm findfwd_xpm finish_xpm interrupt_xpm kill_xpm lock_xpm lookup_xpm \
  next_xpm nexti_xpm mini-folder_xpm mini-ofolder_xpm package_xpm paint_xpm \
  plot_xpm print_xpm rotate_xpm run_xpm set_xpm show_xpm start_xpm step_xpm \
  stepi_xpm stop_xpm subprogram_xpm trash_xpm undisplay_xpm up_xpm \
  var_xpm watch_xpm
	gcc -c $(CFLAGS) $<

clean:
	rm -f *.o *.ali *~ b~*
