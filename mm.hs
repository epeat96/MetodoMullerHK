
muller :: (Floating a, Ord a, Integral b) => (a->a)->a->a->a->a->b->Either [Char] a
muller f x0 x1 x2 tol iteraciones=
    let h1= x1-x0
        h2= x2-x1
        d1= (f(x1)-f(x0))/h1
        d2= (f(x2)-f(x1))/h2
        d= (d2-d1)/(h2+h1)
        b= d2 +h2*d
        _D= ((b^2)-(4*f(x2)*d))**(1/2)
        iter = maxIt iteraciones
    in
        if iter == 0 then
            Left "Se han llegado al limite de iteraciones sin llegar a una respuesta"
        else
                if abs(b-_D) < abs(b+_D) then 
                    let  _E=b+_D
                         h= -2*f(x2)/_E
                         p=x2+h
                    in  
                        if abs(h)<tol then 
                            Right p
                        else
                            muller f x1 x2 p tol iter
                else 
                    let  _E=b-_D
                         h= -2*f(x2)/_E
                         p=x2+h
                    in 
                        if abs(h)<tol then 
                            Right p
                        else
                            muller f x1 x2 p tol iter

maxIt :: (Integral a, Ord a) => a -> a
maxIt x = x - 1


main=do
    print( muller ((\x->x^3-2*x^2-9*x+18)::Double->Double) (-5::Double) (-4::Double) (10::Double) (0.0001::Double) (6::Int)  ) 
