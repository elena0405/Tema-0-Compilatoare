(* Think of these as abstract classes *)
class Comparator {
    compareTo(o1 : Object, o2 : Object):Int {0};
};

class Filter {
    filter(o : Object):Bool {true};
};

(* TODO: implement specified comparators and filters*)

class PriceComparator inherits Comparator {
    (*Compar obiectele o1 si o2 in functie de pret.
      Intorc 0 daca cele doua obiecte au acelasi pret, 
      1 daca primul obiect are pretul mai mic decat 
      al doilea si 2 daca primul obiect are pretul mai 
      mare decat al doilea.*)
    compareTo(o1 : Object, o2 : Object) : Int {
       let price_1 : Int,
           price_2 : Int
        
        in
        {
            price_1 <- case o1 of
                            soda : Soda => soda.getprice();
                            coffee : Coffee => coffee.getprice();
                            laptop : Laptop => laptop.getprice();
                            router : Router => router.getprice();
                        esac;

            price_2 <- case o2 of
                            soda : Soda => soda.getprice();
                            coffee : Coffee => coffee.getprice();
                            laptop : Laptop => laptop.getprice();
                            router : Router => router.getprice();
                        esac;

            if price_1 = price_2
            then
                0
            else if price_1 < price_2
            then
                1
            else 
                2
            fi fi;
        }
    };
};

class RankComparator inherits Comparator {
    (*Compar obiectele o1 si o2 in functie de rang, 
      adica in functie de tipul acestora. Intorc 0 daca 
      cele doua obiecte au acelasi rang, 1 daca primul 
      obiect are rangul mai mic decat al doilea si 2 daca 
      primul obiect are rangul mai mare decat al doilea.*)
    compareTo(o1 : Object, o2 : Object) : Int {
        if o1.type_name() = o2.type_name()
        then
            0
        else
        {
            if o1.type_name() = (new Private).type_name()
            then
                1
            else if o1.type_name() = (new Officer).type_name()
            then
                2
            else
            {
                if o1.type_name() = (new Corporal).type_name()
                then
                    if o2.type_name() = (new Private).type_name()
                    then
                        2
                    else
                        1
                    fi
                else if o1.type_name() = (new Sergent).type_name()
                then
                    if o2.type_name() = (new Officer).type_name()
                    then
                        1
                    else
                        2
                    fi
                else
                    0
                fi fi;
            }
            fi fi;
        }
        fi
    };
};

class AlphabeticComparator inherits Comparator {
    (*Compar obiectele o1 si o2 in functie de valoarea lor.
      Intorc 0 daca cele doua string-uri sunt identice,
      1 daca primul string este mai mic decat al doilea 
      si 2 daca primul string este mai mare decat al doilea.*)
    compareTo(o1 : Object, o2 : Object) : Int {
        case o1 of
            string_1 : String => case o2 of
                                    string_2 : String => if string_1 = string_2 
                                                         then 
                                                            0 
                                                         else if string_1 < string_2
                                                         then
                                                            1
                                                         else
                                                            2
                                                         fi fi;
                                 esac;
        esac
    };
};

class ProductFilter inherits Filter {
    (*Intorc true daca obiectul primit ca parametru
      este de tip Product si false in caz contrar.*)
    filter(o : Object) : Bool {
        if o.type_name() = (new Soda).type_name()
        then
            true
        else if o.type_name() = (new Coffee).type_name()
        then
            true
        else if o.type_name() = (new Laptop).type_name()
        then
            true
        else if o.type_name() = (new Router).type_name()
        then
            true
        else 
            false
        fi fi fi fi
    };
};

class RankFilter inherits Filter {
    (*Intorc true daca obiectul primit ca parametru 
      este de tip Rank si false in caz contrar.*)
    filter(o : Object) : Bool {
        if o.type_name() = (new Private).type_name()
        then
            true
        else if o.type_name() = (new Corporal).type_name()
        then
            true
        else if o.type_name() = (new Sergent).type_name()
        then
            true
        else if o.type_name() = (new Officer).type_name()
        then
            true
        else
            false
        fi fi fi fi
    };
};

class SamePriceFilter inherits Filter {
    (*Intorc true daca obiectul primit ca parametru are
      acelasi pret cu pretul castat la clasa Product si false
      in caz contrar.*)
    filter (o : Object) : Bool {
        case o of
            soda   : Soda   => if soda.getprice() = soda@Product.getprice() then true else false fi;
            coffee : Coffee => if coffee.getprice() = coffee@Product.getprice() then true else false fi;
            laptop : Laptop => if laptop.getprice() = laptop@Product.getprice() then true else false fi;
            router : Router => if router.getprice() = router@Product.getprice() then true else false fi;
            object : Object => false;
        esac
    };
};