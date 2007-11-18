c       $Id$
c       $Log$
c       
c       !
c       Concatenates the string [str2] to the end of string [str1],
c       eliminating leading and trailing blanks.  It can handle either
c       argument being blank; if both are blank, it will return ' '.  Embedded
c       blanks are retained.
c       !
      character*320 function concat(str1,str2)
      character*(*) str1,str2
c       find index of first and last non blank characters
      indst1=1
      len1=len(str1)
      do while (indst1.le.len1.and.str1(indst1:indst1).eq.' ')
        indst1=indst1+1
      enddo
      if(indst1.le.len1)then
        indnd1=len1
        do while (str1(indnd1:indnd1).eq. ' ')
          indnd1=indnd1-1
        enddo
      endif
      indst2=1
      len2=len(str2)
      do while (indst2.le.len2.and.str2(indst2:indst2).eq.' ')
        indst2=indst2+1
      enddo
      if(indst2.le.len2)then
        indnd2=len2
        do while (str2(indnd2:indnd2).eq. ' ')
          indnd2=indnd2-1
        enddo
      endif
      if(indst1.gt.len1.and.indst2.gt.len2)then
        concat=' '
      else if(indst1.gt.len1)then
        concat=str2(indst2:indnd2)
      else if(indst2.gt.len2)then
        concat=str1(indst1:indnd1)
      else
        concat=str1(indst1:indnd1)//str2(indst2:indnd2)
      endif
      return
      end
