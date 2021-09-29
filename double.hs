doubleMe x = x + x  

doubleUs x y = x*2 + y*2

doubleSmall x = if x > 100
                   then (x) + 1
                   else (x*2) + 1

let lostNumbers = [4,8,12,16]