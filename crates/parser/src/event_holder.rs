use crate::event::Event;

#[derive(Debug, Default)]
pub struct EventHolder {
    events: Vec<Event>,
    ignored: Vec<(usize, Event)>,
}

impl EventHolder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_mut(&mut self, pos: usize) -> Option<&mut Event> {
        self.events.get_mut(pos)
    }

    pub fn ignore(&mut self, event: Event) {
        let index = self.events.len();
        self.ignored.push((index, event));
    }

    pub fn push(&mut self, event: Event) {
        self.events.push(event);
    }

    pub fn checkpoint(&mut self) -> usize {
        self.events.len()
    }

    pub fn include_ignored(&mut self) {
        let capacity = self.events.len() + self.ignored.len();
        let mut merged = Vec::with_capacity(capacity);

        let mut left: usize = 0;

        for (ix, event) in self.ignored.iter() {
            merged.extend_from_slice(&self.events[left..*ix]);
            merged.push(event.clone());
            left = *ix;
        }
        merged.extend_from_slice(&self.events[left..]);

        self.events = merged;
    }
}

impl Into<Vec<Event>> for EventHolder {
    fn into(self) -> Vec<Event> {
        self.events
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::syntax::SyntaxKind;

    #[test]
    fn include_ignored() {
        let ignored = vec![
            (1, Event::new_start_node_with(SyntaxKind::Whitespace)),
            (3, Event::new_start_node_with(SyntaxKind::Whitespace)),
            (5, Event::new_start_node_with(SyntaxKind::Whitespace)),
        ];
        let events = vec![Event::FinishNode; 5];

        let total_length = ignored.len() + events.len();

        let mut event_holder = EventHolder { events, ignored };
        event_holder.include_ignored();

        let event_vector: Vec<Event> = event_holder.into();

        assert_eq!(event_vector.len(), total_length);

        for (ix, event) in event_vector.into_iter().enumerate() {
            if ix == 1 || ix == 4 || ix == 7 {
                assert_eq!(event, Event::new_start_node_with(SyntaxKind::Whitespace));
            } else {
                assert_eq!(event, Event::FinishNode);
            }
        }
    }
}
