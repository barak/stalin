/*
 * Run a command with it's standard output and stderr hooked to a
 * pseudo-tty which we pass on to our stdout.  The point
 * of all of this is so that the command's stdio will not
 * buffer it's output.
 */
#include <termios.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>


/*
 * The following definitions make C more amenable to a purist.
 */
#define	bool	char			/* boolean type */
#define	uint	unsigned int		/* short names for unsigned types */
#define	ulong	unsigned long
#define	ullong	unsigned long long	/* GCC extension */
#define	llong	long long		/* GCC extension */
#define	uchar	unsigned char
#define	ushort	unsigned short int
#define	not	!			/* logical negation operator */
#define	and	&&			/* logical conjunction */
#define	or	||			/* logical disjunction */
#define	TRUE	(0 == 0)
#define	FALSE	(not TRUE)
#define	loop	while (TRUE)		/* loop until break */
#define	EOS	'\0'			/* end-of-string char */
#ifndef	NULL
#define	NULL	0			/* invalid pointer */
#endif

#define	unless(p)	if (not (p))
#define	until(p)	while (not (p))
#define	cardof(a)	(sizeof(a) / sizeof(*(a)))
#define	endof(a)	((a) + cardof(a))
#define	bitsof(a)	(sizeof(a) * 8)


#define	STDOUT	1		/* standard output file descriptor */
#define	STDERR	2		/* standard error file descriptor */


static bool	pty();


/*
 * Run a subsidiary command in such a way that it's standard output will
 * not be buffered.
 */
int
main(argc, argv)
int	argc;
char	*argv[];
{
	int		mfd,
			sfd,
			len;
	char		buff[BUFSIZ];
	struct termios	tbuff;

	if (*++argv == NULL) {
		fprintf(stderr, "Usage: unbuff command ...\n");
		exit(1);
	}
	unless (pty(&mfd, &sfd)) {
		fprintf(stderr, "Can't make pty/tty pair\n");
		exit(1);
	}
	switch (fork()) {
	case -1:
		fprintf(stderr, "Try again\n");
		exit(1);
	case 0:
		close(mfd);
		dup2(sfd, STDOUT);
		dup2(sfd, STDERR);
		close(sfd);
		tcgetattr(STDOUT, &tbuff);
		tbuff.c_oflag &= ~ OPOST;
		tcsetattr(STDOUT, TCSANOW, &tbuff);
		execvp(*argv, argv);
		fprintf(stderr, "Can't find %s\n", *argv);
		exit(1);
	default:
		close(sfd);
		loop {
			len = read(mfd, buff, BUFSIZ);
			if (len <= 0)
				break;
			write(STDOUT, buff, len);
		}
	}
	return (0);
}


/*
 * This procedure assumes that pseudo-tty's are named
 *	/dev/pty[p-s][0-9a-f]		master side
 *	/dev/tty[p-s][0-9a-f]		slave side
 */
static bool
pty(mfdp, sfdp)
int	*mfdp,
	*sfdp;
{
	char		*first,
			*second;
	static char	master[]	= "/dev/pty??",
			slave[]		= "/dev/tty??";

	for (first = "pqrs"; *first != EOS; ++first) {
		endof(master)[-3] = *first;
		endof(slave)[-3] = *first;
		for (second = "0123456789abcdef"; *second != EOS; ++second) {
			endof(master)[-2] = *second;
			*mfdp = open(master, 2);
			if (*mfdp >= 0) {
				endof(slave)[-2] = *second;
				*sfdp = open(slave, 2);
				if (*sfdp >= 0)
					return (TRUE);
				close(*mfdp);
			}
		}
	}
	return (FALSE);
}
