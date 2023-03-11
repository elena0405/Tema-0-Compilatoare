
class List {
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

    (*In aceasta metoda fac merge intre doua liste,
      adaugand la finalul listei curente, self, lista
      primita ca si parametru.*)
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

    (*In aceasta metoda, selectez elementele unei liste in 
      functie de un anumit criteriu.*)
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

    (*In aceasta metoda extrag un element dintr-o lista. Voi folosi
      functia in  metoda sortBy.*)
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

    (*In aceasta metoda implementez sortarea unei liste, conform unei
      metode de sortare (crescator sau descrescator) si aplicand un 
      anumit comparator.*)
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
                -- Compar fiecare obiect din lista curenta cu toate
                -- celelalte obiecte neselectate din lista. 
                -- Retin elementele sortate intr-o lista noua.
                aux_list_2 <- aux_list_1;
                aux_obj <- aux_list_1.head();
                
                while (not (aux_list_2.isEmpty())) loop {
                    number <- comparator.compareTo(aux_obj, aux_list_2.head());

                    if sorting_method = "ascendent"
                    then
                    {
                        -- Daca se doreste sortarea crescatoare a elementelor
                        -- listei curente, compar elementul curent cu 
                        -- toate elementele din lista initiala, mai putin
                        -- cu cele selectate deja, si retin intr-o lista noua 
                        -- minimul dintre ele.
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
                        -- Daca se doreste sortarea descrescatoare a elementelor
                        -- listei curente, compar elementul curent cu 
                        -- toate elementele din lista initiala, mai putin
                        -- cu cele selectate deja, si retin intr-o lista noua 
                        -- maximul dintre ele.
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

                -- Adaug elementul minim/maxim in lista noua si il
                -- elimin din lista initiala.
                new_list <- new_list.add(aux_obj);
                aux_list_1 <- extract_one_element_from_list(aux_obj, aux_list_1);
            }
            pool;

            new_list;
        }
    };
};
