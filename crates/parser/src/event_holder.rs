use syntax::syntax_kind::SyntaxKind;
use thin_vec::ThinVec;

use crate::event::Event;

#[derive(Debug, Default)]
pub struct EventHolder {
    events: ThinVec<Event>,
    ignored: ThinVec<(usize, Event)>,
}

impl EventHolder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn update_corresponding_marker_event(&mut self, checkpoint: usize, kind: SyntaxKind) {
        let corresponding_event = self.events.get_mut(checkpoint).unwrap();
        corresponding_event.validate_marker_event(checkpoint);
        *corresponding_event = Event::new_start_node_with(kind);
        self.push(Event::FinishNode);
    }

    pub fn add_forward_parent_marker_event(
        &mut self,
        checkpoint: usize,
        forward_parent_index: usize,
    ) {
        match self.events.get_mut(checkpoint) {
            // add_forward_parent_to_start_node panics if it is not a start node
            Some(event) => event.add_forward_parent_to_start_node(forward_parent_index),
            None => unreachable!(),
        }
    }

    pub fn ignore(&mut self, event: Event) {
        let index = self.events.len();
        self.ignored.push((index, event));
    }

    pub fn push(&mut self, event: Event) {
        self.events.push(event);
    }

    pub fn push_marker_event(&mut self) -> usize {
        let checkpoint = self.checkpoint();
        let event = Event::Marker {
            checkpoint: checkpoint,
        };
        self.events.push(event);
        checkpoint
    }

    pub fn checkpoint(&mut self) -> usize {
        self.events.len()
    }

    pub fn include_ignored(&mut self) {
        let capacity = self.events.len() + self.ignored.len();
        let mut merged = ThinVec::with_capacity(capacity);

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

impl From<EventHolder> for ThinVec<Event> {
    fn from(val: EventHolder) -> Self {
        val.events
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use thin_vec::thin_vec;

    use syntax::syntax_kind::SyntaxKind;

    #[test]
    fn include_ignored() {
        let ignored = thin_vec![
            (1, Event::new_start_node_with(SyntaxKind::Whitespace)),
            (3, Event::new_start_node_with(SyntaxKind::Whitespace)),
            (5, Event::new_start_node_with(SyntaxKind::Whitespace)),
        ];
        let events = thin_vec![Event::FinishNode; 5];

        let total_length = ignored.len() + events.len();

        let mut event_holder = EventHolder { events, ignored };
        event_holder.include_ignored();

        let event_vector: ThinVec<Event> = event_holder.into();

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
