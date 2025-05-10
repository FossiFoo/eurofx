\ Utils

: h. hex . decimal ;
: b. binary . decimal ;
\ : where ' h. ;

: list ( -- )
  cr
  dictionarystart
  begin
    dup 6 + ctype space
    dictionarynext
  until
  drop
;
