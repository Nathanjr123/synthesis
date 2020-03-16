module Synthesis

let abelar x =
    x>12 && x<3097 && (x%12=0)
 
let area b h =
    match ((b >=0.0) && (h>=0.0)) with
    | true -> (b * h)/2.0
    | _ -> failwith "Negative values not permitted!"
let zollo x =
    match (x>0) with
    |false -> x*(-1)
    |_ -> 2*x

let min x y =
    match (x>y) with
    |true -> y
    |_ ->x

let max x y =
    match (x<y) with
    |true -> y
    |_ ->x



let ofTime h m s =
    h*(60*60) + m*60 + s

let toTime s =
    match s<0 with 
    | true->(0,0,0)
    |_->(s/3600,(s%3600)/60,((s%3600)%60))

let digits n = 
    let rec numdig x count = 
        match x>9 with
         |false -> count + 1
         |_->numdig (x/10) (count+1)
    numdig (int (((float n)**2.0)**0.5)) 0

let minmax tuple =
   let a,b,c,d = tuple
   match true with
   |_ -> min (min a b ) (min c d ) , max (max a b) (max c d)
   
   
  

   


let isLeap x =
    match x>1581 with
    |true -> (x%4=0 && x%100<>0) || (x%4=0 && x%100=0 && x%400=0)
    |_ -> failwith "Should be greater than 1582"


let month m =
    failwith "Not implemented"
let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"