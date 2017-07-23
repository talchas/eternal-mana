#[macro_use]
extern crate maplit;
extern crate rand;
extern crate arrayvec;
extern crate histogram;

use histogram::Histogram;
use std::io::Read;
use rand::Rng;
use Card::*;
use Faction::*;
use std::collections::HashMap;
use arrayvec::ArrayVec;

#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Debug)]
enum Faction {
    Fire = 0, Time, Justice, Primal, Shadow
}

#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Default, Debug)]
struct Cost {
    power: u8,
    infl: [u8; 5],
}

// ignoring warp for now
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug)]
enum Card {
    Sigil(Faction, bool),
    Seat(Faction, Faction),
    Banner(Faction, Faction),
    Stranger(Faction, Faction),
    Monument(Faction),
    DiploSeal,
    Tutor { cost: Cost, targets: [bool; 5], depleted: bool, ramp: bool, echo: bool },
    Ramp(Cost),
}

#[derive(Clone, Default, Debug)]
struct Deck {
    relevant: Vec<Card>,
    irrelevant: usize,
}

#[derive(Clone, Copy, Debug)]
struct Success {
    turn: u32,
    had_power: u32,
    with_banner_last_turn: bool,
}

impl From<usize> for Faction {
    fn from(x: usize) -> Faction {
        match x { 0 => Fire, 1 => Time, 2 => Justice, 3 => Primal, 4 => Shadow, _ => unreachable!() }
    }
}

impl Cost {
    fn unlocked(&self, pool: Cost) -> bool {
        self.infl.iter().zip(pool.infl.iter()).all(|(us, pool)| us <= pool)
    }
    fn can_play(&self, pool: Cost) -> bool {
        self.power <= pool.power && self.unlocked(pool)
    }
}

impl Card {
    fn favor(faction: Faction) -> Card {
        let mut target = [false; 5];
        target[faction as usize] = true;
        Tutor { cost: (2, &[faction]).into(), targets: target, depleted: false, ramp: false, echo: false }
    }
    fn is_actual_power(&self) -> bool {
        match *self {
            Stranger(..) => false,
            Tutor{..} => false,
            Ramp(..) => false,
            _ => true,
        }
    }
    fn cost(&self) -> Option<Cost> {
        match *self {
            Stranger(..) => Some((2, &[]).into()),
            Tutor{ cost, ..} => Some(cost),
            Ramp(cost) => Some(cost),
            _ => None,
        }
    }
    fn draws_power(&self) -> bool {
        match *self {
            Tutor{ ramp: false, ..} => true,
            _ => false,
        }
    }
    fn ramp(&self) -> bool {
        match *self {
            Tutor{ ramp: true, ..} => true,
            Ramp(..) => true,
            _ => false,
        }
    }
    fn depleted(&self, have_sigil: bool) -> bool {
        match *self {
            Sigil(_, x) => x,
            Seat(..) => have_sigil,
            Banner(..) => false,
            Monument(..) => true,
            DiploSeal => true,
            Tutor { depleted, ..} => depleted,
            _ => panic!()
        }
    }
}

impl<'a, A: IntoIterator<Item = &'a Faction>> From<(u8, A)> for Cost {
    #[inline]
    fn from(x: (u8, A)) -> Cost {
        let mut cost = Cost { power: x.0, infl: Default::default() };
        for i in x.1 {
            cost.infl[*i as usize] += 1;
        }
        cost
    }
}

fn parse_decklist(deckstr: &str) -> Deck {
    let map: HashMap<&'static str, Card> = hashmap!{
        "Set0 #3" =>  Card::favor(Fire),
        "Set0 #11" =>  Card::favor(Time),
        "Set0 #18" =>  Card::favor(Justice),
        "Set0 #24" =>  Card::favor(Primal),
        "Set0 #35" =>  Card::favor(Shadow),

        "Set1 #408" => Tutor{ cost: (1, &[]).into(), targets: [true; 5], depleted: false, ramp: false, echo: false }, // Seek Power

        "Set1 #74" => Ramp((1, &[Time]).into()), // Initiate of the Sands
        "Set1 #93" =>  Tutor{ cost: (3, &[Time]).into(), targets: [true; 5], depleted: false, ramp: false, echo: false }, // Amber Acolyte
        "Set1 #81" =>  Tutor{ cost: (3, &[Time]).into(), targets: [true; 5], depleted: true, ramp: true, echo: false }, // Secret Pages
        "Set1 #513" =>  Tutor{ cost: (2, &[Time]).into(), targets: [true; 5], depleted: true, ramp: false, echo: true }, // Find the Way
        "Set2 #56" =>  Tutor{ cost: (5, &[Time]).into(), targets: [true; 5], depleted: true, ramp: true, echo: false }, // Amaran Archaeologist
        "Set2 #46" => Ramp((3, &[Time]).into()), // Avirax Familiar

        "Set0 #51" => Seat(Justice, Primal),
        "Set0 #53" => Seat(Fire, Primal),
        "Set0 #54" => Seat(Fire, Time),
        "Set0 #55" => Seat(Justice, Shadow),
        "Set0 #56" => Seat(Fire, Justice),
        "Set0 #58" => Seat(Time, Justice),
        "Set0 #60" => Seat(Fire, Shadow),
        "Set0 #61" => Seat(Time, Shadow),
        "Set0 #62" => Seat(Primal, Shadow),
        "Set0 #63" => Seat(Time, Primal),

        "Set1 #417" => Banner(Primal, Shadow),
        "Set1 #419" => Banner(Fire, Shadow),
        "Set1 #421" => Banner(Time, Primal),
        "Set1 #424" => Banner(Time, Justice),
        "Set1 #427" => Banner(Fire, Justice),
        "Set2 #171" => Banner(Fire, Time),
        "Set2 #186" => Banner(Fire, Primal),
        "Set2 #201" => Banner(Time, Shadow),
        "Set2 #216" => Banner(Justice, Primal),
        "Set2 #231" => Banner(Justice, Shadow),

        "Set1 #409" => Stranger(Primal, Shadow),
        "Set1 #410" => Stranger(Time, Justice),
        "Set1 #411" => Stranger(Fire, Justice),
        "Set1 #412" => Stranger(Time, Primal),
        "Set1 #413" => Stranger(Fire, Shadow),
        "Set2 #246" => Stranger(Justice, Shadow),
        "Set2 #247" => Stranger(Justice, Primal),
        "Set2 #248" => Stranger(Fire, Time),
        "Set2 #250" => Stranger(Fire, Primal),
        "Set2 #251" => Stranger(Time, Shadow),

        "Set1 #418" => Monument(Primal),
        "Set1 #420" => Monument(Time),
        "Set1 #422" => Monument(Justice),
        "Set1 #423" => Monument(Fire),
        "Set1 #426" => Monument(Shadow),

        "Set1 #425" => DiploSeal,

        "Set1 #1" => Sigil(Fire, false),
        "Set1 #63" => Sigil(Time, false),
        "Set1 #126" => Sigil(Justice, false),
        "Set1 #187" => Sigil(Primal, false),
        "Set1 #249" => Sigil(Shadow, false),


    };

    let mut deck = Deck::default();

    for line in deckstr.lines() {
        print!("{}", line);
        let line = line.split(|x| x == '(' || x == ')').collect::<Vec<_>>();
        if line.len() != 3 { continue };
        let count = line[0].split(' ').nth(0).unwrap().parse::<usize>().unwrap();
        match map.get(line[1]) {
            Some(c) => {
                print!(" => {:?}", c);
                for _ in 0..count { deck.relevant.push(*c) }
            }
            None => deck.irrelevant += count,
        }
        println!();
    }
    deck
}

fn parse_cost(s: &str) -> Cost {
    let chars: &[_] = &['F', 'T', 'J', 'P', 'S'];
    let i = s.find(chars).unwrap_or(s.len());
    let (p, inf) = s.split_at(i);
    Cost {
        power: p.trim().parse().unwrap(),
        infl: chars.iter().map(|c| inf.chars().filter(|x| x == c).count() as u8).collect::<ArrayVec<[_; 5]>>().into_inner().unwrap(),
    }
}

impl Deck {
    fn draw<R: Rng>(&mut self, mut rng: R) -> Option<Card> {
        let i = rng.gen_range(0, self.irrelevant + self.relevant.len());
        if i < self.relevant.len() {
            Some(self.relevant.swap_remove(i))
        } else {
            self.irrelevant -= 1;
            None
        }
    }

    fn mulligan<R: Rng>(&mut self, mut rng: R, hand: &mut Vec<Card>) {
        self.irrelevant += 7 - hand.len();
        self.relevant.extend(hand.drain(..));
        let power = rng.gen_range(2, 5); // [2, 4]
        rng.shuffle(&mut self.relevant);
        for _ in 0..power {
            let mut i = 0;
            loop {
                if self.relevant[i].is_actual_power() {
                    hand.push(self.relevant.swap_remove(i));
                    break;
                } else {
                    i += 1;
                }
            }
        }
    }
}

struct State<R> {
    rng: R,
    deck: Deck,
    target: Cost,
    hand: Vec<Card>,
    pool: Cost,
    current_power: u8,
}

impl<R: Rng> State<R> {
    fn draw(&mut self) {
        if let Some(c) = self.deck.draw(&mut self.rng) {
            self.hand.push(c);
        }
    }

    fn current_target(&self) -> Cost {
        if !self.hand.iter().any(|x| x.is_actual_power()) && !self.hand.iter().any(|x| x.draws_power() && x.cost().unwrap().unlocked(self.pool)) {
            // could go with "best" of these
            if let Some(c) = self.hand.iter().filter(|x| x.draws_power()).min_by_key(|x| x.cost().unwrap().power) {
                return c.cost().unwrap();
            }
        }
        if self.pool.power + 1 < self.target.power {
            if !self.hand.iter().any(|x| x.ramp() && x.cost().unwrap().unlocked(self.pool)) {
                if let Some(c) = self.hand.iter().filter(|x| x.draws_power()).min_by_key(|x| x.cost().unwrap().power) {
                    return c.cost().unwrap();
                }
            }
        }
        self.target
    }

    fn missing(&self, target: Cost) -> [u8; 5] {
        target.infl.iter().zip(self.pool.infl.iter()).map(|(x, y)| x.saturating_sub(*y)).collect::<ArrayVec<_>>().into_inner().unwrap()
    }
/*
    fn new_influence(&self, target: Faction, card: Card) -> [u8; 5] {
        let mut infl = self.pool.infl;
        {
            let mut go = |x: Faction| infl[x as usize] += 1;
            match card {
                Sigil(x, _) | Monument(x) => go(x),
                Seat(x,y) | Banner(x, y) | Stranger(x, y) => {go(x); go(y)},
                DiploSeal => if self.pool.infl.iter().sum::<u8>() <= 2 { go(target) },
                _ => panic!(),
            }
        }
        infl
    }
*/
    fn power_influence_benefit(&self, target: Cost, card: Card) -> u8 {
        let m = self.missing(target);
        if m.iter().sum::<u8>() == 0 {
            return 0;
        }

        let val = |x: Faction| std::cmp::min(m[x as usize], 1);
        match card {
            Sigil(x, _) | Monument(x) => val(x),
            Seat(x,y) | Banner(x, y) => val(x) + val(y),
            Tutor {cost, ..} => cost.can_play(self.pool) as u8,
            DiploSeal => (self.pool.infl.iter().sum::<u8>() <= 2) as u8,
            _ => panic!(),
        }
    }

    fn play(&mut self, i: usize, target: Faction, have_sigil: bool) -> Option<usize> {
        let diplo = self.pool.infl.iter().sum::<u8>() <= 2;
        let infl = &mut self.pool.infl;
        let mut go = |x: Faction| infl[x as usize] += 1;
        match self.hand.swap_remove(i) {
            Sigil(x, depleted) => {
                go(x);
                self.pool.power += 1;
                self.current_power += !depleted as u8;
                None
            },
            Seat(x, y) => {
                go(x); go(y);
                self.pool.power += 1;
                self.current_power += have_sigil as u8;
                None
            }
            Banner(x, y) => {
                go(x); go(y);
                self.pool.power += 1;
                self.current_power += 1;
                None
            },
            Stranger(x, y) => {
                go(x); go(y);
                self.current_power -= 2;
                None
            },
            Monument(x) => {
                go(x);
                self.pool.power += 1;
                None
            },
            DiploSeal => {
                if diplo { go(target); }
                self.pool.power += 1;
                self.current_power += 1;
                None
            },
            Tutor { cost, targets, depleted, ramp, echo } => {
                self.current_power -= cost.power;
                let faction = if targets[target as usize] { target } else { targets.iter().position(|x| *x).unwrap().into() };

                if echo {
                    self.hand.push(Tutor {echo: false, cost, targets, depleted, ramp});
                }
                if ramp {
                    go(faction);
                    self.pool.power += 1;
                    self.current_power += !depleted as u8;
                    None
                } else {
                    self.hand.push(Sigil(faction, depleted));
                    Some(self.hand.len() - 1)
                }
            },
            Ramp(c) => {
                self.pool.power += 1;
                self.current_power -= c.power;
                None
            },
        }
    }

    fn target_faction(&mut self, target: Cost) -> Faction {
        let mut m = self.missing(target);
        if m.iter().sum::<u8>() == 0 {
            m = self.missing(self.target);
            if m.iter().sum::<u8>() == 0 {
                return Fire;
            }
        }
        let mut x = m.iter().cloned().enumerate().collect::<ArrayVec<[_; 5]>>();
        self.rng.shuffle(&mut x);
        x.into_iter().max_by_key(|a| a.1).unwrap().0.into()
    }

    fn run(&mut self, max_turns: u32) -> Option<Success> {
        for _ in 0..7 {
            self.draw();
        }
        if self.hand.len() > 5 || self.hand.iter().filter(|x| x.is_actual_power()).count() < 2 {
            self.deck.mulligan(&mut self.rng, &mut self.hand);
        }
        //println!("{:?}", self.hand);

        let mut had_power = None;
        let mut previous_pool = Cost::default();
        for turns in 0..max_turns {
            self.draw();
            self.current_power = self.pool.power;
            let target = self.current_target();
            let mut was_banner = false;

            let have_sigil = self.hand.iter().any(|x| match *x {
                Sigil(..) => true,
                Tutor{ cost, .. } => cost.can_play(previous_pool),
                _ => false,
            });
            
            if let Some((i, c)) = self.hand.iter().cloned().enumerate().filter(|x| x.1.is_actual_power() || x.1.cost().map(|x| x.can_play(self.pool)).unwrap_or(false)).
                max_by(|&(_,x),&(_,y)| {
                    let diplo = if self.pool.infl.iter().sum::<u8>() == 2 {
                        (x == DiploSeal).cmp(&(y == DiploSeal))
                    } else {
                        std::cmp::Ordering::Equal
                    };
                    let depleted = (x.depleted(have_sigil) as u8).cmp(&(x.depleted(have_sigil) as u8));
                    let cost = x.cost().map(|c| c.power).cmp(&y.cost().map(|c| c.power));
                    let expensive = cost.then(depleted);
                    diplo.then(self.power_influence_benefit(target, x).cmp(&self.power_influence_benefit(target, x)))
                        .then(if self.pool.power + 1 == target.power { expensive } else { expensive.reverse() })})
            {
                //print!("{:?}", c);
                let faction = self.target_faction(target);
                if let Some(i) = self.play(i, faction, have_sigil) {
                    self.play(i, faction, have_sigil);
                }
                if c.cost().map(|x| x.can_play(self.pool)).unwrap_or(false) {
                    self.current_power = self.pool.power;
                }
                match c {
                    Banner(..) => was_banner = true,
                    _ => (),
                }
            } else {
            }

            //println!(" -- {} {:?}", self.current_power, self.pool);
            

            
            if self.pool.power >= self.target.power && self.current_power >= self.target.power {
                had_power = had_power.or(Some(turns));
                if self.missing(self.target) == [0; 5] {
                    return Some(Success {
                        turn: turns + 1,
                        had_power: had_power.unwrap() + 1,
                        with_banner_last_turn: was_banner,
                    });
                }
            }


            // todo, ramp, strangers, better diplo seal
            
            if self.pool.power >= 5 {
                self.hand.retain(|x| match *x { Monument(_) => false, _ => true });
            }
            previous_pool = self.pool;
        }
        None
    }
        
}

fn print_histogram(name: &str, h: &Histogram, target: u64) {
    let get = |i| {
        (0..(i+1)).map(|i| h.get(i).unwrap_or(0) as f32).sum::<f32>() / h.entries() as f32
    };
    println!("{} {:.3} {:.3} {:.3}  | {:2} {:2} {:2} {:2} {:2}", name,
             get(target), get(target + 1), get(target + 2),
             h.percentile(60.).unwrap(), h.percentile(70.).unwrap(),
             h.percentile(80.).unwrap(), h.percentile(90.).unwrap(), h.percentile(95.).unwrap());
}

fn main() {
    let target = parse_cost(&std::env::args().nth(1).unwrap());
    println!("{:?}", target);
    let mut decklist = String::new();
    std::io::stdin().read_to_string(&mut decklist).unwrap();
    let deck = parse_decklist(&decklist);

    let max_turns = target.power as u32 * 3 + 3;

    let mut rng = rand::thread_rng();
    let config = Histogram::configure().max_value(max_turns as u64 + 1);
    let mut had_power = config.clone().build().unwrap();
    let mut had_colors = config.clone().build().unwrap();
    let mut had_colors_no_banner = config.clone().build().unwrap();
    for _ in 0..5000 {
        let mut state = State {
            rng: &mut rng,
            deck: deck.clone(),
            target: target,
            hand: vec![],
            pool: Cost::default(),
            current_power: 0
        };

        let res = state.run(max_turns).unwrap_or(Success { turn: max_turns + 1, had_power: max_turns + 1, with_banner_last_turn: false });
        //println!("{:?} {:?} {}/{}", res, state.hand, state.deck.relevant.len(), state.deck.irrelevant);

        
        had_power.increment(res.had_power as u64).unwrap();
        had_colors.increment(res.turn as u64).unwrap();
        had_colors_no_banner.increment(res.turn as u64 + res.with_banner_last_turn as u64).unwrap();

    }
    let target = had_power.minimum().unwrap();
    println!("      {:5} {:5} {:5}  | 60 70 80 90 95", target, target + 1, target + 2);
    
    print_histogram("power", &had_power, target);
    print_histogram(" infl", &had_colors, target);
    print_histogram("!unit", &had_colors_no_banner, target);
}
