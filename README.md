# lambda_expression
(\x.x x) -> x <br /> 
(\x.x) -> Error <br /> 
((\x.\y.(x y) x) y) -> (x y ) # для двух аргументов <br /> 
(\x.x \y.y) x y -> λv2.v2 # \y.y будет вставлен \x.x, а x y игнорируется <br /> 
