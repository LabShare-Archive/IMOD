        integer ibufreal
        parameter (ibufreal = 100000000)
        integer*4 nbufsiz
        real*4 array(ibufreal)
        COMMON/FTBUF/ NBUFSIZ,ARRAY
