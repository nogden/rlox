#![allow(dead_code)]

#[derive(Clone, Debug)]
struct Node {
    next: Option<usize>,
    previous: Option<usize>,
    value: String,
}

#[derive(Clone, Debug)]
struct LinkedList {
    head: Option<usize>,
    elements: Vec<Node>
}

impl LinkedList {
    fn new() -> LinkedList {
        LinkedList { head: None, elements: Vec::new() }
    }

    fn len(&self) -> usize {
        self.elements.len()
    }

    fn push(&mut self, item: String) {
        let new_head_index = self.elements.len();
        self.elements.push(Node {
            next: self.head,
            previous: None,
            value: item
        });

        if let Some(old_head_index) = self.head {
            let old_head = self.elements.get_mut(old_head_index).unwrap();
            old_head.previous = Some(new_head_index);
        }

        self.head = Some(new_head_index);
    }

    fn pop(&mut self) -> Option<String> {
        if self.head.is_none() {
            return None
        }

        let head_index = self.head.unwrap();
        let head = self.elements.remove(head_index);
        self.head = if let Some(next_index) = head.next {
            let new_head = self.elements.get_mut(next_index).unwrap();
            new_head.previous = None;
            Some(head_index)
        } else {
            None
        };

        Some(head.value)
    }

    fn get(&self, index: usize) -> Option<&str> {
        if self.head.is_none() {
            return None
        }

        let mut node = self.elements.get(self.head.unwrap()).unwrap();
        if index == 0 {
            return Some(node.value.as_ref())
        }

        let mut list_index = 0;
        while list_index < index {
            if let Some(node_index) = node.next {
                node = self.elements.get(node_index).unwrap();
            } else {
                return None
            }
            list_index += 1;
        }

        Some(node.value.as_ref())
    }

    fn index_of(&self, value: &str) -> Option<usize> {
        if self.head.is_none() {
            return None
        }

        let mut list_index = 0;

        let mut node = self.elements.get(self.head.unwrap()).unwrap();
        if node.value == value {
            return Some(list_index)
        }

        while let Some(next_index) = node.next {
            list_index += 1;
            node = self.elements.get(next_index).unwrap();
            if node.value == value {
                return Some(list_index)
            }
        }

        None
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn newly_created_lists_are_empty() {
        let list = LinkedList::new();
        assert_eq!(list.len(), 0)
    }

    #[test]
    fn items_are_pushed_to_the_head_of_the_list() {
        let mut list = list_of_length(3);
        assert_eq!(list.get(0).unwrap(), "Item 3");

        list.push("Item 4".to_owned());
        assert_eq!(list.get(0).unwrap(), "Item 4");
    }

    #[test]
    fn adding_an_item_increases_the_length_of_the_list_by_1() {
        let mut list = LinkedList::new();
        list.push("Item 1".to_owned());
        assert_eq!(list.len(), 1);

        list.push("Item 2".to_owned());
        assert_eq!(list.len(), 2);

        list.push("Item 3".to_owned());
        assert_eq!(list.len(), 3);
    }

    #[test]
    fn items_are_popped_from_the_head_of_the_list() {
        let mut list = list_of_length(3);
        let popped_item = list.pop().unwrap();
        assert_eq!(popped_item, "Item 3");
    }

    #[test]
    fn popping_an_item_reduces_the_length_of_the_list_by_1() {
        let mut list = list_of_length(3);
        let _ = list.pop();
        assert_eq!(list.len(), 2);
    }

    #[test]
    fn popping_an_empty_list_yields_none() {
        let mut list = LinkedList::new();
        assert!(list.pop().is_none());
    }

    #[test]
    fn the_first_index_of_an_inserted_item_can_be_found() {
        let mut list = list_of_length(3);
        assert_eq!(list.index_of("Item 2").unwrap(), 1);

        list.push("Item 2".to_owned());
        assert_eq!(list.index_of("Item 2").unwrap(), 0);
    }

    #[test]
    fn indexing_out_of_bounds_yields_none() {
        let list = list_of_length(3);
        assert!(list.get(3).is_none());
    }

    #[test]
    fn looking_up_the_index_of_any_other_item_yields_none() {
        let list = list_of_length(3);
        assert!(list.index_of("Not in list").is_none());
    }

    fn list_of_length(item_count: usize) -> LinkedList {
        let mut list = LinkedList::new();
        for n in 0..item_count {
            list.push(format!("Item {}", n+1));
        }
        list
    }
}
