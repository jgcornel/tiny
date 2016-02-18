./tiny < $1 > why.s
as why.s
ld a.out -o w
./w
#echo $?
rm -f ./w ./a.out
