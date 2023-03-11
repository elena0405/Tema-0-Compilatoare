class Main inherits IO {
    lists        : List <- new List;
    element_list : List;
    looping      : Bool <- true;
    somestr      : String;
    command      : String;
    idx          : Int <- 0;

    (*In aceasta functie extrag dintr-un string primit
      ca parametru cuvantul intalnit pana la spatiu, 
      incepand cu pozitia idx.*)
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

    (*In aceasta functie afisez comanda help cu 
      explicatiile aferente.*)
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

    (*In aceasta metoda verific daca tipul
      primit ca si parametru este unul din tipurile
      care mostenesc clasa Rank.*)
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

    (*In aceasta metoda verific daca tipul primit 
      ca si parametru este unul din tipurile care
      mostenesc clasa Product.*)
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

    (*In aceasta metoda verific daca tipul primit
      ca si parametru este unul din tipurile de baza
      ale limbajului COOL.*)
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

    (*In aceasta metoda creez un element de tipul Product.*)
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

    (*In aceasta metoda creez un obiect de tipul Rank.*)
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

    (*In aceasta metoda creez un element de unul dintre tipurile de
      baza ale limbajului COOL.*)
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

    (*In aceasta metoda extrag elementul aflat pe pozitia nr
      din lista list.*)
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

    (*In aceasta metoda adaug elementele citite pana la 
      intalnirea sirului "END" intr-o lista, iar lista 
      nou creata o adaug in lista de liste lists.*)
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

    (*In aceasta metoda implementez comanda "print".*)
    print_list(command : String) : Object {
        -- Daca vreau sa printez toata lista de liste
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
            -- Daca vreau sa afisez doar o lista din lista de liste,
            -- extrag din sirul citit de la tastatura indexul listei,
            -- determin lista si o afisez.
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

    (*In aceasta metoda implementez comanda merge.*)
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

            -- Extrag din lista de liste cele doua liste pe care le voi concatena.
            element_1 <- extract_x_element_from_list(nr_1, aux_list);
            element_2 <- extract_x_element_from_list(nr_2, aux_list);

            -- In aceasta bucla while adaug intr-o lista noua
            -- toate listele, in afara de cele doua liste la care voi
            -- face merge.
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

            -- Concatenez cele doua liste folosind metoda merge din clasa List
            -- si adaug rezultatul la finalul listei nou create.
            case element_1 of
                list_1 : List => case element_2 of
                                    list_2 : List => new_list <- new_list.add(list_1.merge(list_2));
                                esac;
            esac;
            
            -- Fac elementul lists sa refere lista nou creata.
            lists <- new_list;
        }
    };

    (*In aceasta metoda implementez comanda "filter".*)
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

            -- Extrag lista pe care vreau sa o filtrez.
            element <- extract_x_element_from_list(nr, aux_list);

            -- Creez un nou filtru pentru ea.
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

            -- Creez o lista noua in care voi retine toate listele vechi,
            -- cu exceptia celei pe care am vrut sa o filtrez, inlocuind-o
            -- cu lista nou filtrata.
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

    -- In aceasta metoda implementez comanda sort.
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

            -- Extrag lista pe care vreau sa o sortez.
            element <- extract_x_element_from_list(nr, aux_list);

            -- Creez un comparator nou pentru ea.
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

            -- Creez o lista noua in care adaug listele vechi, mai putin
            -- lista pe care am ales-o pentru a fi sortat-o, inlocuind-o 
            -- cu rezultatul obtinut in urma sortarii.
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
            -- Incarc in lista elementele initiale.
            add_elements_to_a_list();
            -- Citesc comanda.
            command <- in_string();
            
            while (not command = "") loop {
                -- Initializez cu 0 indexul cu care voi itera prin comanda,
                -- pentru a extrage argumentele acesteia.
                idx <- 0;
                -- Pentru a nu primi eroare in urma extragerii cuvintelor din
                -- sirul command pana la caracterul " ", adaug la finalul 
                -- sirului caracterul " ".
                command <- command.concat(" ");
                -- Extrag primul cuvant din comanda.
                somestr <- extract_one_element_until_space(command);

                -- In functie de valoarea cuvantului, aplic functia aferenta.
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
};