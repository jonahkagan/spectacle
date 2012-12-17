let flip (f : 'a -> 'b -> 'c) : ('b -> 'a -> 'c) = (fun a b -> f b a)
