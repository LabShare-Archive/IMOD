C       
C       July 10, 2000 CER
C       
C       Create degree versions of the trignometric functions to enable
C       the Linux port
C       
C       
      real function SIND(degrees)
      SIND = SIN(degrees * 0.01745329252)
      END

      real function ASIND(a)
      ASIND = ASIN(a) * 57.2957795131
      END

      real function COSD(degrees)
      COSD = COS(degrees * 0.01745329252)
      END

      real function ACOSD(a)
      ACOSD = ACOS(a) * 57.2957795131
      END

      real function TAND(degrees)
      TAND = TAN(degrees * 0.01745329252)
      END

      real function ATAND(a)
      ATAND = ATAN(a) * 57.2957795131
      END

      real function ATAN2D(a1,a2)
      ATAN2D = ATAN2(a1,a2) * 57.2957795131
      END
      
