
BEGIN {
    FS=",|\\(|\\)"
}

{ 
    split($0, a);
    printf "(%s", $1 
    for(i = 2; i < NF; i++) printf " %s", a[i] 
    printf ")\n"
}
