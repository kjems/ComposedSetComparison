namespace Common

module List =
    let rec startsWith l1 l2 =
      match l1, l2 with     
      | [],[] | _, [] -> true
      | x::xs, y::ys when x = y -> startsWith xs ys
      | _ -> false

    let rec endsWith l1 l2 = 
        startsWith (List.rev l1) (List.rev l2)    

    let sub xs startIndex count =
        let rec sub xs c i acc = 
            match c,i with
            | c,_ when c >= count      -> List.rev acc
            | _,i when i <  startIndex -> sub xs c (i+1) acc
            | _,i when i >= startIndex -> 
                match xs with
                | []    -> List.rev acc
                | x::xs -> sub xs (c+1) (i+1) (x::acc)
            | _ -> []  // should not happen
        sub xs 0 0 []
