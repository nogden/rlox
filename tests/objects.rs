mod utils;

use rlox::Value::*;

use utils::*;

#[test]
fn classes_print_as_their_type_name() {
    assert_eq!("<class MyType>\n", lox_stdout!{
        class MyType {}
        print MyType;
    });
}

#[test]
fn objects_may_be_instansiated_by_calling_the_type_name_as_a_constructor() {
    assert_eq!("<object MyType>\n", lox_stdout!{
        class MyType {}
        var instance = MyType();
        print instance;
    });
}

#[test]
fn object_fields_may_be_defined_via_dot_notation() {
    assert_eq!(Some(Number(2.0)), lox!{
        class MyType {}
        var instance = MyType();
        instance.value = 2;
    });
}

#[test]
fn object_fields_may_be_accessed_via_dot_notation() {
    assert_eq!(Some(Number(2.0)), lox!{
        class MyType {}
        var instance = MyType();
        instance.value = 2;
        instance.value
    });
}

#[test]
fn object_methods_may_be_called_via_dot_notation() {
    assert_eq!("Hello\n", lox_stdout!{
        class MyType {
            greet() {
                print "Hello";
            }
        }

        MyType().greet();
    });
}

#[test]
fn methods_may_use_this_to_refer_to_the_object_on_which_they_are_invoked() {
    assert_eq!("The German chocolate cake is delicious!\n", lox_stdout!{
        class Cake {
            taste() {
                var adjective = "delicious";
                print "The " + this.flavor + " cake is " + adjective + "!";
            }
        }

        var cake = Cake();
        cake.flavor = "German chocolate";
        cake.taste();
    });
}

#[test]
fn methods_are_first_class_and_remember_the_object_on_which_they_were_defined() {
    assert_eq!("Red socks\n", lox_stdout!{
        class Gift {
            callback() {
                fun localFunction() {
                    print this.colour + " socks";
                }

                return localFunction;
            }
        }

        var gift = Gift();
        var callback = gift.callback();
        gift.colour = "Red";

        callback();
    });
}

#[test]
#[should_panic(expected = "'this' cannot be used outside of an object")]
fn referencing_this_outside_of_an_object_is_an_error() {
    lox!{
        print this;
    };
}

#[test]
fn object_constructors_are_called_on_creation_if_defined() {
    assert_eq!("Red socks\n", lox_stdout!{
        class Gift {
            init() {
                this.colour = "Red";
            }

            display() {
                print this.colour + " socks";
            }
        }

        var gift = Gift();
        gift.display();
    });
}

#[test]
fn class_creation_arity_matches_constructor_arity() {
    assert_eq!("Red socks\n", lox_stdout!{
        class Gift {
            init(colour) {
                this.colour = colour;
            }

            display() {
                print this.colour + " socks";
            }
        }

        var gift = Gift("Red");
        gift.display();
    });
}

#[test]
fn calling_init_directly_reinitialises_the_object_returning_a_new_reference() {
    assert_eq!("Red socks\nBlue socks\nGreen socks\nGreen socks\n", lox_stdout!{
        class Gift {
            init(colour) {
                this.colour = colour;
            }

            display() {
                print this.colour + " socks";
            }
        }

        var gift = Gift("Red");
        gift.display();

        gift.colour = "Blue";
        gift.display();

        var reinit_gift = gift.init("Green");
        gift.display();
        reinit_gift.display();
    });
}

#[test]
fn explicitly_returning_from_a_constructor_is_allowed() {
    assert_eq!("Red\n", lox_stdout!{
        class Gift {
            init(colour) {
                this.colour = "Red";
                return;
                this.colour = colour;
            }

            display() {
                print this.colour;
            }
        }

        var gift = Gift("Blue");
        gift.display();
    });
}

#[test]
#[should_panic(expected = "Cannot return a value from a constructor")]
fn explicitly_retuning_a_value_in_a_constructor_is_an_error() {
    lox!{
        class Gift {
            init(colour) {
                return "Nonsense";
            }
        }
    };
}
