



pub type ObjectId = usize;
pub enum Direction { Up, Down }

pub struct Node {
    upnode: usize,
    downnode: usize,
    x :Box<NodeTrait>,
}

pub struct Train {}

pub trait NodeTrait {
    fn arrive_front(&self, train :&Train) {}
    fn arrive_back(&self, train :&Train) {}
}

pub struct Signal {
    green: Observable<bool>,
    authority: Observable<f64>,
    dir: Direction,
}
impl NodeTrait for Signal {}

pub struct Detector {
    touched: Observable<()>,
    up_tvd :ObjectId,
    down_tvd :ObjectId,
}

pub struct TVD {
    occupied :Observable<bool>,
}


pub struct Railway {
    pub nodes: Vec<Node<f64>>,
    pub objects: Vec<Object>,
}

impl NodeTrait for Detector {
    fn arrive_front(&self, sim :&mut Simulation<Railway>) {
        if train.get_dir() == Direction::Up {
            match world.objects.get_mut(self.down_tvd) {
                Some(tvd) => tvd.occupied.set(sim, true);
            }
        }
    }
}

