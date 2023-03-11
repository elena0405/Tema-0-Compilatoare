(*
   The class A2I provides integer-to-string and string-to-integer
conversion routines.  To use these routines, either inherit them
in the class where needed, have a dummy variable bound to
something of type A2I, or simpl write (new A2I).method(argument).
*)


(*
   c2i   Converts a 1-character string to an integer.  Aborts
         if the string is not "0" through "9"
*)
class A2I {

     c2i(char : String) : Int {
        if char = "0" then 0 else
        if char = "1" then 1 else
        if char = "2" then 2 else
        if char = "3" then 3 else
        if char = "4" then 4 else
        if char = "5" then 5 else
        if char = "6" then 6 else
        if char = "7" then 7 else
        if char = "8" then 8 else
        if char = "9" then 9 else
        { abort(); 0; }  -- the 0 is needed to satisfy the typchecker
        fi fi fi fi fi fi fi fi fi fi
     };

(*
   i2c is the inverse of c2i.
*)
     i2c(i : Int) : String {
        if i = 0 then "0" else
        if i = 1 then "1" else
        if i = 2 then "2" else
        if i = 3 then "3" else
        if i = 4 then "4" else
        if i = 5 then "5" else
        if i = 6 then "6" else
        if i = 7 then "7" else
        if i = 8 then "8" else
        if i = 9 then "9" else
        { abort(); ""; }  -- the "" is needed to satisfy the typchecker
            fi fi fi fi fi fi fi fi fi fi
     };

(*
   a2i converts an ASCII string into an integer.  The empty string
is converted to 0.  Signed and unsigned strings are handled.  The
method aborts if the string does not represent an integer.  Very
long strings of digits produce strange answers because of arithmetic 
overflow.

*)
     a2i(s : String) : Int {
        if s.length() = 0 then 0 else
        if s.substr(0,1) = "-" then ~a2i_aux(s.substr(1,s.length()-1)) else
            if s.substr(0,1) = "+" then a2i_aux(s.substr(1,s.length()-1)) else
            a2i_aux(s)
            fi fi fi
     };

(*
  a2i_aux converts the usigned portion of the string.  As a programming
example, this method is written iteratively.
*)
     a2i_aux(s : String) : Int {
        (let int : Int <- 0 in  
            {    
                (let j : Int <- s.length() in
                (let i : Int <- 0 in
                while i < j loop
                {
                    int <- int * 10 + c2i(s.substr(i,1));
                    i <- i + 1;
                }
                pool
            )
            );
                int;
            }
            )
     };

(*
    i2a converts an integer to a string.  Positive and negative 
numbers are handled correctly.  
*)
    i2a(i : Int) : String {
        if i = 0 then "0" else 
            if 0 < i then i2a_aux(i) else
            "-".concat(i2a_aux(i * ~1)) 
            fi fi
    };
    
(*
    i2a_aux is an example using recursion.
*)      
    i2a_aux(i : Int) : String {
        if i = 0 then "" else 
        (let next : Int <- i / 10 in
        i2a_aux(next).concat(i2c(i - next * 10))
        )
        fi
    };

};class List {
    head_list : Object;
    tail_list: List;
    empty : Bool;

    (* Creez metoda init, care functioneaza ca un constructor
       in Java, pentru a initializa campurile clasei cu nisie
       valori date ca si parametrii.*)
    init_method(head_param : Object, tail_param : List) : List {
        {
            head_list <- head_param;
            tail_list <- tail_param;
            self;
        }
    };

    head() : Object {
        head_list
    };

    tail() : List {
        tail_list
    };

    (*In aceasta metoda, adaug un element la finalul listei.*)
    add(o : Object) : SELF_TYPE {
        {   
            if isEmpty()
            then
                self.init_method(o, new List)
            else
                tail_list.add(o)
            fi;

            self;
        }
    };

    (*In aceasta metoda, verific daca o lista este vida.*)
    isEmpty() : Bool {
        {
            if isvoid head_list
            then
                true
            else
                false
            fi;
        }
    };

    (*In aceasta metoda, concatenez lista curenta, self, cu o lista 
      primita ca si parametru.*)
    append(l1 : List) : List {
        {
            if isEmpty() = true
            then
                l1
            else
                tail_list.append(l1)
            fi;
        }
    };

    (*In aceasta metoda, convertesc un obiect la tipul String.
      Voi folosi aceasta functie la metoda toString aferenta 
      listei curente.*)
    object_to_string(object : Object) : String {
        let new_string : String <- ""

        in
        {
            case object of
                soda     : Soda     => new_string <- soda.toString();
                coffee   : Coffee   => new_string <- new_string.concat(coffee.toString());
                laptop   : Laptop   => new_string <- new_string.concat(laptop.toString());
                router   : Router   => new_string <- new_string.concat(router.toString());
                private  : Private  => new_string <- new_string.concat(private.toString());
                corporal : Corporal => new_string <- new_string.concat(corporal.toString());
                sergent  : Sergent  => new_string <- new_string.concat(sergent.toString());
                officer  : Officer  => new_string <- new_string.concat(officer.toString());
                int      : Int      => {new_string <- new_string.concat("Int("); new_string <- new_string.concat((new A2I).i2a(int)); new_string <- new_string.concat(")");};
                string   : String   => {new_string <- new_string.concat("String("); new_string <- new_string.concat(string); new_string <- new_string.concat(")");};
                bool     : Bool     => {new_string <- new_string.concat("Bool("); if bool = true then new_string <- new_string.concat("true") else new_string <- new_string.concat("false") fi; new_string <- new_string.concat(")");};
                io       : IO       => new_string <- new_string.concat("IO()");
            esac;

            new_string;
        }
    };

    (*In aceasta metoda afisez o lista din lista de 
      liste construita.*)
    print_x_line_from_list(l : List) : String {
        let new_string : String,
            iterator   : Object

        in
        {
            new_string <- new_string.concat("[ ");

            while (not l.isEmpty()) loop {
                iterator <- l.head();
                l <- l.tail();
                new_string <- new_string.concat(object_to_string(iterator));

                if (not (l.isEmpty() = true))
                then
                    new_string <- new_string.concat(", ")
                else
                    new_string <- new_string.concat("")
                fi;
            }
            pool;

            new_string <- new_string.concat(" ]");
            new_string;
        }
    };

    (*Implementez aici metoda toString, pentru a afisa 
      elementele unei liste.*)
    toString() : String {
        let aux_list : List <- new List,
            nr       : Int <- 1,
            element  : Object,
            str      : String

        in
        {
            aux_list <- self;
            
            -- Verific daca lista curenta este nevida.
            if (not aux_list.isEmpty())
            then
                -- Daca da, verific ce tip de elemente are.
                if aux_list.head().type_name() = (new List).type_name()
                then
                    -- Daca elementele listei sunt tot liste, consturiesc formatul
                    -- aferent si apelez metoda de afisare a unei liste pentru fiecare 
                    -- element al listei initiale, anume metoda print_x_line_from_list.
                    case aux_list.head() of
                    list : List => {
                                        while (not (aux_list.isEmpty() = true)) loop {
                                            str <- str.concat((new A2I).i2c(nr));
                                            str <- str.concat(": ");
                                            element <- aux_list.head();

                                            case element of
                                                list : List => str <- str.concat(print_x_line_from_list(list));
                                            esac;

                                            str <- str.concat("\n");
                                            nr <- nr + 1;
                                            aux_list <- aux_list.tail();
                                        }
                                        pool;
                                    };
                    esac
                else
                {
                    -- Daca elementele listei nu sunt tot liste, ci simple
                    -- obiecte, apelez o singura data metoda care afiseaza
                    -- o lista de obiecte.
                    str <- print_x_line_from_list(aux_list);
                    str <- str.concat("\n");
                }

                fi
            else
                -- Daca lista pe care vreau sa o afisez este vida, 
                -- construiesc formatul corespunzator.
                str <- str.concat("[  ]\n")
            fi;

            str;
        }
    };

    merge(other : List) : List {
        let aux_list : List <- new List

        in
        {
            aux_list <- self;
            
            if isEmpty() = true
            then
                aux_list <- other
            else
            {
                while (not (other.isEmpty())) loop {
                    aux_list <- aux_list.add(other.head());
                    other <- other.tail();
                }
                pool;
            }
            fi;

            aux_list;
        }
    };

    filterBy(filter_param : Filter) : List {
        let check_filter : Bool,
            new_list     : List <- new List,
            aux_list     : List <- new List

        in
        {
            aux_list <- self;
            
            while (not (aux_list.isEmpty())) loop {
                check_filter <- filter_param.filter(aux_list.head());

                if check_filter = true
                then
                {
                    if new_list.isEmpty() = true
                    then
                        new_list <- new_list.init_method(aux_list.head(), new List)
                    else
                        new_list <- new_list.add(aux_list.head())
                    fi;

                    aux_list <- aux_list.tail();
                }
                else
                    aux_list <- aux_list.tail()
                fi;
            }
            pool;

            new_list;
        }
    };

    extract_one_element_from_list(o : Object, l : List) : List {
        let new_list : List <- new List

        in
        {
            while (not (l.isEmpty() = true)) loop {
                if o = l.head()
                then
                    l <- l.tail()
                else
                {
                    new_list <- new_list.add(l.head());
                    l <- l.tail();
                }
                fi;
            }
            pool;

            new_list;
        }
    };

    sortBy(comparator : Comparator, sorting_method : String) : List {
        let number     : Int,
            aux_obj    : Object,
            new_list   : List <- new List,
            aux_list_1 : List <- new List,
            aux_list_2 : List <- new List

        in
        {
            aux_list_1 <- self;

            while (not (aux_list_1.isEmpty())) loop {
                aux_list_2 <- aux_list_1;
                aux_obj <- aux_list_1.head();
                
                while (not (aux_list_2.isEmpty())) loop {
                    number <- comparator.compareTo(aux_obj, aux_list_2.head());

                    if sorting_method = "ascendent"
                    then
                    {
                        if number = 2
                        then
                        {
                            aux_obj <- aux_list_2.head();
                            aux_list_2 <- aux_list_2.tail();
                        }
                        else
                            aux_list_2 <- aux_list_2.tail()
                        fi;
                    }
                    else
                    {
                        if number = 1
                        then
                        {
                            aux_obj <- aux_list_2.head();
                            aux_list_2 <- aux_list_2.tail();
                        }
                        else
                            aux_list_2 <- aux_list_2.tail()
                        fi;
                    }
                    fi;
                }
                pool;

                new_list <- new_list.add(aux_obj);
                aux_list_1 <- extract_one_element_from_list(aux_obj, aux_list_1);
            }
            pool;

            new_list;
        }
    };
};class Main inherits IO {
    lists        : List <- new List;
    element_list : List;
    looping      : Bool <- true;
    somestr      : String;
    command      : String;
    idx          : Int <- 0;

    extract_one_element_until_space(s : String) : String {
        let new_string : String <- "",
            index : Int <- 0

        in
        {   
            while (not (s.substr(idx + index, 1) = " ")) loop 
                {
                    new_string <- new_string.concat(s.substr(idx + index, 1));
                    index <- index + 1;
                } 
            pool;

            idx <- idx + index + 1;

            new_string;
        }
    };

    display_help_command() : Object {
        {
            out_string("\n");
            out_string("The possible commands are:");
            out_string("\n");
            out_string("1. help: displays brief information about each command");
            out_string("\n");
            out_string("2. load: creates a list of elements");
            out_string("\n");
            out_string("3. print: prints all the existing lists");
            out_string("\n");
            out_string("4. merge: concatenate one list at the end of another list");
            out_string("\n");
            out_string("5. filterBy: filters the elements from a list based on certain criteria");
            out_string("\n");
            out_string("6. sortBy: sorts the elements from a list based on a given function");  
            out_string("\n"); 
        }
    };

    check_Rank_element(type : String) : Bool {
        {
            if type = (new Private).type_name()
            then
                true
            else if type = (new Corporal).type_name()
            then
                true
            else if type = (new Sergent).type_name()
            then
                true
            else if type = (new Officer).type_name()
            then
                true
            else 
                false
            fi fi fi fi;
        }
    };

    check_Product_element(type : String) : Bool {
        {
            if type = (new Soda).type_name()
            then
                true
            else if type = (new Coffee).type_name()
            then
                true
            else if type = (new Laptop).type_name()
            then
                true
            else if type = (new Router).type_name()
            then
                true
            else
                false
            fi fi fi fi;
        }
    };

    check_basic_element(type : String) : Bool {
        {
            if type = "String"
            then
                true
            else if type = (new Int).type_name()
            then
                true
            else if type = (new Bool).type_name()
            then
                true
            else
                false
            fi fi fi;
        }
    };

    create_Product_element(type : String, name_param : String, model_param : String, price_param : Int) : Object {
        let element : Object

        in
        {
            if type = (new Soda).type_name()
            then
                element <- (new Soda).init(name_param, model_param, price_param)
            else if type = (new Coffee).type_name()
            then
                element <- (new Coffee).init(name_param, model_param, price_param)
            else if type = (new Laptop).type_name()
            then
                element <- (new Laptop).init(name_param, model_param, price_param)
            else if type = (new Router).type_name()
            then
                element <- (new Router).init(name_param, model_param, price_param)
            else
                abort()
            fi fi fi fi;

            element;
        }
    };

    create_Rank_element(type : String, name_param : String) : Object {
        let element : Object 

        in
        {
            if type = (new Private).type_name()
            then
                element <- (new Private).init(name_param)
            else if type = (new Corporal).type_name()
            then
                element <- (new Corporal).init(name_param)
            else if type = (new Sergent).type_name()
            then
                element <- (new Sergent).init(name_param)
            else if type = (new Officer).type_name()
            then
                element <- (new Officer).init(name_param)
            else
                abort()
            
            fi fi fi fi;

            element;
        }
    };

    create_basic_element(type : String, value : String) : Object {
        let element : Object

        in
        {
            if type = (new String).type_name()
            then
                element <- value
            else if type = (new Int).type_name()
            then
                element <- (new A2I).a2i(value)
            else
                if value = "true"
                then
                    element <- true
                else
                    element <- false
                fi
            fi fi;

            element;
        }
    };

    extract_x_element_from_list(nr : Int, list : List) : Object {
        let count : Int <- 0,
            element : Object
        
        in
        {
            while (not (count = nr)) loop {
                element <- list.head();
                list <- list.tail();
                count <- count + 1;
            }
            pool;

            element;
        }
    };

    add_elements_to_a_list() : Object {
        let element            : Object,
            is_of_type_Rank    : Bool,
            is_of_type_Product : Bool,
            is_of_basic_type   : Bool,
            str_1              : String,
            str_2              : String,
            str_3              : String,
            str_4              : String,
            nr                 : Int
        
        in
        {
            element_list <- new List;
            somestr <- in_string();
            somestr <- somestr.concat(" ");

            while (not (somestr = "END ")) loop {
                idx <- 0;
                str_1 <- extract_one_element_until_space(somestr);
                is_of_type_Rank <- check_Rank_element(str_1);
                is_of_type_Product <- check_Product_element(str_1);
                is_of_basic_type <- check_basic_element(str_1);
                    
                if is_of_type_Rank = true
                then
                {
                    str_2 <- extract_one_element_until_space(somestr);
                    element <- create_Rank_element(str_1, str_2);
                }
                else if is_of_type_Product = true
                then
                {
                    str_2 <- extract_one_element_until_space(somestr);
                    str_3 <- extract_one_element_until_space(somestr);
                    str_4 <- extract_one_element_until_space(somestr);
                    nr <- (new A2I).a2i(str_4);
                    element <- create_Product_element(str_1, str_2, str_3, nr);
                }
                else if is_of_basic_type = true
                then
                {
                    str_2 <- extract_one_element_until_space(somestr);
                    element <- create_basic_element(str_1, str_2);
                }
                else if str_1 = (new IO).type_name()
                then
                    element <- new IO
                else
                    abort()
                fi fi fi fi;

                element_list <- element_list.add(element);
                somestr <- in_string();
                somestr <- somestr.concat(" ");
            }
            pool;

            lists <- lists.add(element_list);
        }
    };

    print_list(command : String) : Object {
        if command = "print "
        then
        {
            let aux_list : List <- new List,
            element  : Object,
            str      : String <- "",
            nr       : Int <- 1
                        
            in
            {
                str <- lists.toString();
                out_string(str);
            };
        }
        else
        {
            let str : String,
                nr : Int,
                aux_list : List <- new List,
                element : Object
                            
            in
            {
                str <- extract_one_element_until_space(command);
                nr <- (new A2I).a2i(str);
                aux_list <- lists;
                element <- extract_x_element_from_list(nr, aux_list);

                case element of
                    list : List => out_string(list.toString());
                esac;
            };
        }
        fi
    };

    merge_command(command : String) : Object {
        let str_1       : String,
            str_2       : String,
            nr_1        : Int,
            nr_2        : Int,
            element_1   : Object,
            element_2   : Object,
            new_list    : List <- new List,
            aux_list    : List,
            count       : Int <- 1

        in
        {
            str_1 <- extract_one_element_until_space(command);
            str_2 <- extract_one_element_until_space(command);
            nr_1  <- (new A2I).c2i(str_1);
            nr_2  <- (new A2I).c2i(str_2);
            aux_list <- lists;

            element_1 <- extract_x_element_from_list(nr_1, aux_list);
            element_2 <- extract_x_element_from_list(nr_2, aux_list);

            while (not (aux_list.isEmpty() = true)) loop {
                if count = nr_1
                then
                    aux_list <- aux_list.tail()
                else if count = nr_2
                then
                    aux_list <- aux_list.tail()
                else 
                {
                    new_list <- new_list.add(aux_list.head());
                    aux_list <- aux_list.tail();
                }
                fi fi;

                count <- count + 1;
            }
            pool;

            case element_1 of
                list_1 : List => case element_2 of
                                    list_2 : List => new_list <- new_list.add(list_1.merge(list_2));
                                esac;
            esac;

            lists <- new_list;
        }
    };

    filter_command(command : String) : List {
        let str_1         : String,
            str_2         : String,
            nr            : Int,
            count         : Int <- 1,
            aux_list      : List <- new List,
            new_list      : List <- new List,
            filtered_list : List,
            f             : Filter,
            element       : Object

        in
        {
            aux_list <- lists;
            
            str_1 <- extract_one_element_until_space(command);
            str_2 <- extract_one_element_until_space(command);
            nr <- (new A2I).c2i(str_1);

            element <- extract_x_element_from_list(nr, aux_list);

            if str_2 = (new ProductFilter).type_name()
            then
                f <- new ProductFilter
            else if str_2 = (new RankFilter).type_name()
            then
                f <- new RankFilter
            else if str_2 = (new SamePriceFilter).type_name()
            then
                f <- new SamePriceFilter
            else 
                abort()
            fi fi fi;

            aux_list <- lists;

            while (not (aux_list.isEmpty() = true)) loop {
                if count = nr
                then
                {
                    case element of
                        list : List => filtered_list <- list.filterBy(f);
                    esac;
                    
                    new_list <- new_list.add(filtered_list);
                    aux_list <- aux_list.tail();
                }
                else
                {
                    new_list <- new_list.add(aux_list.head());
                    aux_list <- aux_list.tail();
                }
                fi;

                count <- count + 1;
            }   
            pool;

            new_list;
        }
    };

    sort_command(command : String) : List {
        let str_1    : String,
            str_2    : String,
            str_3    : String,
            nr       : Int,
            count    : Int <- 1,
            cmp      : Comparator,
            element  : Object,
            new_list : List <- new List,
            aux_list : List <- new List
        
        in
        {
            aux_list <- lists;
            
            str_1 <- extract_one_element_until_space(command);
            str_2 <- extract_one_element_until_space(command);
            str_3 <- extract_one_element_until_space(command);
            nr <- (new A2I).c2i(str_1);

            element <- extract_x_element_from_list(nr, aux_list);

            if str_2 = (new PriceComparator).type_name()
            then
                cmp <- new PriceComparator
            else if str_2 = (new RankComparator).type_name()
            then
                cmp <- new RankComparator
            else
                cmp <- new AlphabeticComparator
            fi fi;

            aux_list <- lists;

            while (not (aux_list.isEmpty())) loop {
                if count = nr
                then
                    case element of
                        list : List => new_list <- new_list.add(list.sortBy(cmp, str_3));
                    esac
                else
                    new_list <- new_list.add(aux_list.head())
                fi;

                aux_list <- aux_list.tail();
                count <- count + 1;
            }
            pool;

            new_list;
        }
    };

    main() : Object {
        {
            add_elements_to_a_list();
            command <- in_string();
            
            while (not command = "") loop {
                idx <- 0;
                command <- command.concat(" ");
                somestr <- extract_one_element_until_space(command);

                if somestr = "help" 
                then
                    display_help_command()
                else if somestr = "load"
                then
                    add_elements_to_a_list()
                else if somestr = "print"
                then
                    print_list(command)
                else if somestr = "merge"
                then
                    merge_command(command)
                else if somestr = "filterBy"
                then
                    lists <- filter_command(command)
                else if somestr = "sortBy"
                then
                    lists <- sort_command(command)
                else
                    abort()
                fi fi fi fi fi fi;

                command <- in_string();
            } pool;
        }
    };
};(*******************************
 *** Classes Product-related ***
 *******************************)
class Product {
    name : String;
    model : String;
    price : Int;

    init(n : String, m: String, p : Int):SELF_TYPE {{
        name <- n;
        model <- m;
        price <- p;
        self;
    }};

    getprice():Int{ price * 119 / 100 };

    toString() : String {
        let new_string : String <- ""

        in
        {
            if type_name() = (new Soda).type_name()
            then
                new_string <- new_string.concat("Soda(")
            else if type_name() = (new Coffee).type_name()
            then
                new_string <- new_string.concat("Coffee(")
            else if type_name() = (new Laptop).type_name()
            then
                new_string <- new_string.concat("Laptop(")
            else
                new_string <- new_string.concat("Router(")
            fi fi fi;

            new_string <- new_string.concat(name);
            new_string <- new_string.concat(",");
            new_string <- new_string.concat(model);
            new_string <- new_string.concat(")");

            new_string;
        }
    };
};

class Edible inherits Product {
    -- VAT tax is lower for foods
    getprice():Int { price * 109 / 100 };
};

class Soda inherits Edible {
    -- sugar tax is 20 bani
    getprice():Int {price * 109 / 100 + 20};
};

class Coffee inherits Edible {
    -- this is technically poison for ants
    getprice():Int {price * 119 / 100};
};

class Laptop inherits Product {
    -- operating system cost included
    getprice():Int {price * 119 / 100 + 499};
};

class Router inherits Product {};

(****************************
 *** Classes Rank-related ***
 ****************************)
class Rank {
    name : String;

    init(n : String):SELF_TYPE {
        {
            name <- n;
            self;
        }
    };

    getname() : String {
        name
    };

    toString() : String {
        -- Hint: what are the default methods of Object?
        let new_string : String <- ""

        in
        {
            if type_name() = (new Private).type_name()
            then
                new_string <- new_string.concat("Private(")
            else if type_name() = (new Corporal).type_name()
            then
                new_string <- new_string.concat("Corporal(")
            else if type_name() = (new Sergent).type_name()
            then
                new_string <- new_string.concat("Sergent(")
            else
                new_string <- new_string.concat("Officer(")
            fi fi fi;

            new_string <- new_string.concat(name);
            new_string <- new_string.concat(")");
            new_string;
        }
    };
};

class Private inherits Rank {};

class Corporal inherits Private {};

class Sergent inherits Corporal {};

class Officer inherits Sergent {};(* Think of these as abstract classes *)
class Comparator {
    compareTo(o1 : Object, o2 : Object):Int {0};
};

class Filter {
    filter(o : Object):Bool {true};
};

(* TODO: implement specified comparators and filters*)

class PriceComparator inherits Comparator {
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