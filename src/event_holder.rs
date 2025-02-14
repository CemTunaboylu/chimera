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
    pub fn get(&mut self, pos: usize) -> Option<&Event> {
        self.events.get(pos)
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
        for (ix, event) in self.ignored.iter() {
            self.events.insert(*ix, event.clone());
        }
    }
}

impl Into<Vec<Event>> for EventHolder {
    fn into(self) -> Vec<Event> {
        self.events
    }
}
