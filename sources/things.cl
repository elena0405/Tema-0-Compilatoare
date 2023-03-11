(*******************************
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

class Officer inherits Sergent {};