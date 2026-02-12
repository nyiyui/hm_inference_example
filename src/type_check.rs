use crate::ast::{BinaryOp, Expr, UnaryOp};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn gensym() -> String {
    let id = GENSYM_COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("${}", id + 1)
}

pub fn reset_gensym() {
    GENSYM_COUNTER.store(0, Ordering::SeqCst);
}

#[derive(Debug, Clone, PartialEq)]
pub enum Typ {
    TVar(String),
    TClosure(Box<Typ>, Box<Typ>),
    TInt,
    TBool,
}

impl fmt::Display for Typ {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Typ::TVar(x) => write!(f, "{}", x),
            Typ::TClosure(t1, t2) => write!(f, "({} -> {})", t1, t2),
            Typ::TInt => write!(f, "int"),
            Typ::TBool => write!(f, "bool"),
        }
    }
}

pub type Subst = HashMap<String, Typ>;
pub type TypeScheme = (Vec<String>, Typ);

fn vars_in(t: &Typ) -> Vec<String> {
    let mut vars = HashSet::new();
    fn collect(t: &Typ, vars: &mut HashSet<String>) {
        match t {
            Typ::TVar(x) => {
                vars.insert(x.clone());
            }
            Typ::TClosure(t1, t2) => {
                collect(t1, vars);
                collect(t2, vars);
            }
            _ => {}
        }
    }
    collect(t, &mut vars);
    let mut result: Vec<_> = vars.into_iter().collect();
    result.sort();
    result
}

fn not_contains(a: &str, t: &Typ) -> bool {
    match t {
        Typ::TVar(b) if b == a => false,
        Typ::TClosure(t1, t2) => not_contains(a, t1) && not_contains(a, t2),
        _ => true,
    }
}

fn apply_typ(s: &Subst, t: &Typ) -> Typ {
    match t {
        Typ::TVar(x) => s.get(x).cloned().unwrap_or_else(|| t.clone()),
        Typ::TClosure(t1, t2) => Typ::TClosure(
            Box::new(apply_typ(s, t1)),
            Box::new(apply_typ(s, t2)),
        ),
        _ => t.clone(),
    }
}

fn compose(s1: &Subst, s2: &Subst) -> Subst {
    let mut result = HashMap::new();
    
    // Apply s2 to each type in s1
    for (x, t) in s1 {
        result.insert(x.clone(), apply_typ(s2, t));
    }
    
    // Add substitutions from s2 that are not in s1
    for (x, t) in s2 {
        if !result.contains_key(x) {
            result.insert(x.clone(), t.clone());
        }
    }
    
    result
}

pub fn lower(t: &Typ) -> Typ {
    let vars = vars_in(t);
    let mut s = HashMap::new();
    
    let mut counter = 0;
    for var in vars {
        counter += 1;
        s.insert(var, Typ::TVar(format!("${}", counter)));
    }
    
    apply_typ(&s, t)
}

pub struct Env {
    bindings: HashMap<String, TypeScheme>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            bindings: HashMap::new(),
        }
    }

    pub fn lookup(&self, x: &str) -> Result<TypeScheme, String> {
        self.bindings
            .get(x)
            .cloned()
            .ok_or_else(|| format!("unbound variable {}", x))
    }

    pub fn extend(&self, x: String, ts: TypeScheme) -> Self {
        let mut new_bindings = self.bindings.clone();
        new_bindings.insert(x, ts);
        Env {
            bindings: new_bindings,
        }
    }
}

fn apply_env(s: &Subst, env: &Env) -> Env {
    let mut new_bindings = HashMap::new();
    for (x, (vars, t)) in &env.bindings {
        new_bindings.insert(x.clone(), (vars.clone(), apply_typ(s, t)));
    }
    Env {
        bindings: new_bindings,
    }
}

fn instantiate(ts: &TypeScheme) -> Typ {
    let (vars, t) = ts;
    let mut s = HashMap::new();
    for var in vars {
        s.insert(var.clone(), Typ::TVar(gensym()));
    }
    apply_typ(&s, t)
}

fn generalize(env: &Env, t: &Typ) -> TypeScheme {
    let vars_in_t: HashSet<_> = vars_in(t).into_iter().collect();
    let mut vars_in_env = HashSet::new();
    for (_, (vars, _)) in &env.bindings {
        for var in vars {
            vars_in_env.insert(var.clone());
        }
    }
    
    let mut vars_not_in: Vec<_> = vars_in_t
        .into_iter()
        .filter(|x| !vars_in_env.contains(x))
        .collect();
    vars_not_in.sort();
    
    (vars_not_in, t.clone())
}

fn unify(t1: &Typ, t2: &Typ) -> Result<Subst, String> {
    match (t1, t2) {
        (Typ::TInt, Typ::TInt) => Ok(HashMap::new()),
        (Typ::TBool, Typ::TBool) => Ok(HashMap::new()),
        (Typ::TVar(x), Typ::TVar(y)) if x == y => Ok(HashMap::new()),
        (Typ::TVar(x), t) if not_contains(x, t) => {
            let mut s = HashMap::new();
            s.insert(x.clone(), t.clone());
            Ok(s)
        }
        (t, Typ::TVar(x)) if not_contains(x, t) => {
            let mut s = HashMap::new();
            s.insert(x.clone(), t.clone());
            Ok(s)
        }
        (Typ::TClosure(lhs1, rhs1), Typ::TClosure(lhs2, rhs2)) => {
            let s1 = unify(lhs1, lhs2)?;
            let rhs1_applied = apply_typ(&s1, rhs1);
            let rhs2_applied = apply_typ(&s1, rhs2);
            let s2 = unify(&rhs1_applied, &rhs2_applied)?;
            Ok(compose(&s1, &s2))
        }
        _ => Err(format!(
            "unification failed between {} and {}",
            t1, t2
        )),
    }
}

pub fn infer(e: &Expr, env: &Env) -> Result<(Typ, Subst), String> {
    match e {
        Expr::Var(x) => {
            let ts = env.lookup(x)?;
            let t = instantiate(&ts);
            Ok((t, HashMap::new()))
        }
        Expr::Int(_) => Ok((Typ::TInt, HashMap::new())),
        Expr::Bool(_) => Ok((Typ::TBool, HashMap::new())),
        Expr::OpUnary(op, e) => {
            let (t, s) = infer(e, env)?;
            match op {
                UnaryOp::Not => {
                    let expected = Typ::TBool;
                    match &t {
                        Typ::TVar(x) => {
                            let mut s_new = HashMap::new();
                            s_new.insert(x.clone(), expected.clone());
                            Ok((expected, compose(&s, &s_new)))
                        }
                        t if t == &expected => Ok((expected, s)),
                        _ => Err(format!("must be bool")),
                    }
                }
                UnaryOp::Neg => {
                    let expected = Typ::TInt;
                    match &t {
                        Typ::TVar(x) => {
                            let mut s_new = HashMap::new();
                            s_new.insert(x.clone(), expected.clone());
                            Ok((expected, compose(&s, &s_new)))
                        }
                        t if t == &expected => Ok((expected, s)),
                        _ => Err(format!("must be int")),
                    }
                }
            }
        }
        Expr::OpBinary(op, e1, e2) => {
            match op {
                BinaryOp::Equal => {
                    let (t1, s1) = infer(e1, env)?;
                    let (t2, s2) = infer(e2, env)?;
                    let s3 = unify(&t1, &t2)?;
                    let s4 = compose(&compose(&s1, &s2), &s3);
                    Ok((Typ::TBool, s4))
                }
                BinaryOp::Add | BinaryOp::Mul => {
                    let expected = Typ::TInt;
                    let (t1, s1) = infer(e1, env)?;
                    let (t2, s2) = infer(e2, env)?;
                    let s3 = compose(&s1, &s2);
                    
                    let mut s_result = s3.clone();
                    match (&t1, &t2) {
                        (Typ::TVar(x), Typ::TVar(y)) if x == y => {
                            s_result.insert(x.clone(), expected.clone());
                        }
                        (Typ::TVar(x), Typ::TVar(y)) => {
                            s_result.insert(x.clone(), expected.clone());
                            s_result.insert(y.clone(), expected.clone());
                        }
                        (Typ::TVar(x), t2) if t2 == &expected => {
                            s_result.insert(x.clone(), expected.clone());
                        }
                        (t1, Typ::TVar(y)) if t1 == &expected => {
                            s_result.insert(y.clone(), expected.clone());
                        }
                        (t1, t2) if t1 == &expected && t2 == &expected => {}
                        _ => {
                            return Err(format!(
                                "both sides must be int but lhs is {} and rhs is {}",
                                t1, t2
                            ))
                        }
                    }
                    Ok((expected, s_result))
                }
                BinaryOp::And | BinaryOp::Or => {
                    let expected = Typ::TBool;
                    let (t1, s1) = infer(e1, env)?;
                    let (t2, s2) = infer(e2, env)?;
                    let s3 = compose(&s1, &s2);
                    
                    let mut s_result = s3.clone();
                    match (&t1, &t2) {
                        (Typ::TVar(x), Typ::TVar(y)) if x == y => {
                            s_result.insert(x.clone(), expected.clone());
                        }
                        (Typ::TVar(x), Typ::TVar(y)) => {
                            s_result.insert(x.clone(), expected.clone());
                            s_result.insert(y.clone(), expected.clone());
                        }
                        (Typ::TVar(x), t2) if t2 == &expected => {
                            s_result.insert(x.clone(), expected.clone());
                        }
                        (t1, Typ::TVar(y)) if t1 == &expected => {
                            s_result.insert(y.clone(), expected.clone());
                        }
                        (t1, t2) if t1 == &expected && t2 == &expected => {}
                        _ => {
                            return Err(format!(
                                "both sides must be bool but lhs is {} and rhs is {}",
                                t1, t2
                            ))
                        }
                    }
                    Ok((expected, s_result))
                }
            }
        }
        Expr::Closure(x, e) => {
            let a = Typ::TVar(gensym());
            let env_prime = env.extend(x.clone(), (vec![], a.clone()));
            let (t, s) = infer(e, &env_prime)?;
            let result_type = Typ::TClosure(Box::new(apply_typ(&s, &a)), Box::new(t));
            Ok((result_type, s))
        }
        Expr::Application(e1, e2) => {
            let a = Typ::TVar(gensym());
            let (t1, s1) = infer(e1, env)?;
            let env1 = apply_env(&s1, env);
            let (t2, s2) = infer(e2, &env1)?;
            let t1_prime = apply_typ(&s2, &t1);
            let t3 = Typ::TClosure(Box::new(t2), Box::new(a.clone()));
            let s3 = unify(&t1_prime, &t3)?;
            let s4 = compose(&compose(&s1, &s2), &s3);
            Ok((apply_typ(&s4, &a), s4))
        }
        Expr::Let(x, e1, e2) => {
            let (t1, s1) = infer(e1, env)?;
            let ts = generalize(env, &apply_typ(&s1, &t1));
            let env_prime = apply_env(&s1, env).extend(x.clone(), ts);
            infer(e2, &env_prime)
        }
        Expr::If(e1, e2, e3) => {
            let (t1, s1) = infer(e1, env)?;
            let (t2, s2) = infer(e2, env)?;
            let (t3, s3) = infer(e3, env)?;
            let s4 = unify(&t1, &Typ::TBool)?;
            let s5 = unify(&t2, &t3)?;
            let s6 = compose(&compose(&compose(&compose(&s1, &s2), &s3), &s4), &s5);
            Ok((apply_typ(&s6, &t2), s6))
        }
    }
}
