  PROGRAM hello_world
    IMPLICIT NONE
    LOGICAL :: to_file
    CHARACTER (LEN=1) :: selection
    
    WRITE(*,*) 'Write to console [c] or file [f]?'
    READ(*,*) selection
    WRITE(*,*) 'Selection: ',selection
    IF (selection=='c') THEN
       WRITE(*,*) 'Hello, World!'
    ELSE
       WRITE(*,*) 'Hello, World! -> File'
    ENDIF
    
  END PROGRAM hello_world
  
