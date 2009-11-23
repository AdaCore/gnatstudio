#pragma message ("home made termio.h included for windows.")

#ifndef termio
  #pragma message ("including struct termios")
  typedef unsigned char cc_t;
  typedef unsigned short tcflag_t;
  typedef unsigned char speed_t;

  struct termios {
	tcflag_t	c_iflag;
	tcflag_t	c_oflag;
	tcflag_t	c_cflag;
	tcflag_t	c_lflag;
	char		c_line;
	cc_t		c_cc[100];
	speed_t		c_ispeed;
	speed_t		c_ospeed;
  };
  #pragma message ("#define termio termios")
  #define termio termios
#endif
