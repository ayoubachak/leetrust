// lib.rs
pub mod chunk;
pub mod value;
pub mod memory;
pub mod debug;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpCode {
    Return,
    Constant,
    ConstantLong,
    Nil,
    True,
    False,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Equal,
    Greater,
    Less,
    Print,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    Jump,
    JumpIfFalse,
    Loop,
    Call,
    Closure,
    CloseUpvalue,
    GetUpvalue,
    SetUpvalue,
    Class,
    Method,
    Invoke,
    Inherit,
    GetProperty,
    SetProperty,
    GetSuper,
    SuperInvoke,
    Import,
    EndModule,
    ReturnModule,
    ImportVariable,
    ImportVariableAs,
    ImportModule,
    ImportModuleAs,
    ImportMethod,
    ImportMethodAs,
    ImportClass,
    ImportClassAs,
    ImportSuperclass,
    ImportSuperclassAs,
    ImportProperty,
    ImportPropertyAs,
    ImportMethodProperty,
    ImportMethodPropertyAs,
    ImportClassProperty,
    ImportClassPropertyAs,
    ImportSuperclassProperty,
    ImportSuperclassPropertyAs,
    String,
}

impl OpCode {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0 => Some(OpCode::Return),
            1 => Some(OpCode::Constant),
            2 => Some(OpCode::ConstantLong),
            3 => Some(OpCode::Nil),
            4 => Some(OpCode::True),
            5 => Some(OpCode::False),
            6 => Some(OpCode::Negate),
            7 => Some(OpCode::Add),
            8 => Some(OpCode::Subtract),
            9 => Some(OpCode::Multiply),
            10 => Some(OpCode::Divide),
            11 => Some(OpCode::Not),
            12 => Some(OpCode::Equal),
            13 => Some(OpCode::Greater),
            14 => Some(OpCode::Less),
            15 => Some(OpCode::Print),
            16 => Some(OpCode::Pop),
            17 => Some(OpCode::DefineGlobal),
            18 => Some(OpCode::GetGlobal),
            19 => Some(OpCode::SetGlobal),
            20 => Some(OpCode::GetLocal),
            21 => Some(OpCode::SetLocal),
            22 => Some(OpCode::Jump),
            23 => Some(OpCode::JumpIfFalse),
            24 => Some(OpCode::Loop),
            25 => Some(OpCode::Call),
            26 => Some(OpCode::Closure),
            27 => Some(OpCode::CloseUpvalue),
            28 => Some(OpCode::GetUpvalue),
            29 => Some(OpCode::SetUpvalue),
            30 => Some(OpCode::Class),
            31 => Some(OpCode::Method),
            32 => Some(OpCode::Invoke),
            33 => Some(OpCode::Inherit),
            34 => Some(OpCode::GetProperty),
            35 => Some(OpCode::SetProperty),
            36 => Some(OpCode::GetSuper),
            37 => Some(OpCode::SuperInvoke),
            38 => Some(OpCode::Import),
            39 => Some(OpCode::EndModule),
            40 => Some(OpCode::ReturnModule),
            41 => Some(OpCode::ImportVariable),
            42 => Some(OpCode::ImportVariableAs),
            43 => Some(OpCode::ImportModule),
            44 => Some(OpCode::ImportModuleAs),
            45 => Some(OpCode::ImportMethod),
            46 => Some(OpCode::ImportMethodAs),
            47 => Some(OpCode::ImportClass),
            48 => Some(OpCode::ImportClassAs),
            49 => Some(OpCode::ImportSuperclass),
            50 => Some(OpCode::ImportSuperclassAs),
            51 => Some(OpCode::ImportProperty),
            52 => Some(OpCode::ImportPropertyAs),
            53 => Some(OpCode::ImportMethodProperty),
            54 => Some(OpCode::ImportMethodPropertyAs),
            55 => Some(OpCode::ImportClassProperty),
            56 => Some(OpCode::ImportClassPropertyAs),
            57 => Some(OpCode::ImportSuperclassProperty),
            58 => Some(OpCode::ImportSuperclassPropertyAs),
            59 => Some(OpCode::String),
            _ => None,
        }
    }

    pub fn to_u8(self) -> u8 {
        match self {
            OpCode::Return => 0,
            OpCode::Constant => 1,
            OpCode::ConstantLong => 2,
            OpCode::Nil => 3,
            OpCode::True => 4,
            OpCode::False => 5,
            OpCode::Negate => 6,
            OpCode::Add => 7,
            OpCode::Subtract => 8,
            OpCode::Multiply => 9,
            OpCode::Divide => 10,
            OpCode::Not => 11,
            OpCode::Equal => 12,
            OpCode::Greater => 13,
            OpCode::Less => 14,
            OpCode::Print => 15,
            OpCode::Pop => 16,
            OpCode::DefineGlobal => 17,
            OpCode::GetGlobal => 18,
            OpCode::SetGlobal => 19,
            OpCode::GetLocal => 20,
            OpCode::SetLocal => 21,
            OpCode::Jump => 22,
            OpCode::JumpIfFalse => 23,
            OpCode::Loop => 24,
            OpCode::Call => 25,
            OpCode::Closure => 26,
            OpCode::CloseUpvalue => 27,
            OpCode::GetUpvalue => 28,
            OpCode::SetUpvalue => 29,
            OpCode::Class => 30,
            OpCode::Method => 31,
            OpCode::Invoke => 32,
            OpCode::Inherit => 33,
            OpCode::GetProperty => 34,
            OpCode::SetProperty => 35,
            OpCode::GetSuper => 36,
            OpCode::SuperInvoke => 37,
            OpCode::Import => 38,
            OpCode::EndModule => 39,
            OpCode::ReturnModule => 40,
            OpCode::ImportVariable => 41,
            OpCode::ImportVariableAs => 42,
            OpCode::ImportModule => 43,
            OpCode::ImportModuleAs => 44,
            OpCode::ImportMethod => 45,
            OpCode::ImportMethodAs => 46,
            OpCode::ImportClass => 47,
            OpCode::ImportClassAs => 48,
            OpCode::ImportSuperclass => 49,
            OpCode::ImportSuperclassAs => 50,
            OpCode::ImportProperty => 51,
            OpCode::ImportPropertyAs => 52,
            OpCode::ImportMethodProperty => 53,
            OpCode::ImportMethodPropertyAs => 54,
            OpCode::ImportClassProperty => 55,
            OpCode::ImportClassPropertyAs => 56,
            OpCode::ImportSuperclassProperty => 57,
            OpCode::ImportSuperclassPropertyAs => 58,
            OpCode::String => 59,
        }
    }

    pub fn from_usize(index: usize) -> Option<Self> {
        match index {
            0 => Some(OpCode::Return),
            1 => Some(OpCode::Constant),
            2 => Some(OpCode::ConstantLong),
            3 => Some(OpCode::Nil),
            4 => Some(OpCode::True),
            5 => Some(OpCode::False),
            6 => Some(OpCode::Negate),
            7 => Some(OpCode::Add),
            8 => Some(OpCode::Subtract),
            9 => Some(OpCode::Multiply),
            10 => Some(OpCode::Divide),
            11 => Some(OpCode::Not),
            12 => Some(OpCode::Equal),
            13 => Some(OpCode::Greater),
            14 => Some(OpCode::Less),
            15 => Some(OpCode::Print),
            16 => Some(OpCode::Pop),
            17 => Some(OpCode::DefineGlobal),
            18 => Some(OpCode::GetGlobal),
            19 => Some(OpCode::SetGlobal),
            20 => Some(OpCode::GetLocal),
            21 => Some(OpCode::SetLocal),
            22 => Some(OpCode::Jump),
            23 => Some(OpCode::JumpIfFalse),
            24 => Some(OpCode::Loop),
            25 => Some(OpCode::Call),
            26 => Some(OpCode::Closure),
            27 => Some(OpCode::CloseUpvalue),
            28 => Some(OpCode::GetUpvalue),
            29 => Some(OpCode::SetUpvalue),
            30 => Some(OpCode::Class),
            31 => Some(OpCode::Method),
            32 => Some(OpCode::Invoke),
            33 => Some(OpCode::Inherit),
            34 => Some(OpCode::GetProperty),
            35 => Some(OpCode::SetProperty),
            36 => Some(OpCode::GetSuper),
            37 => Some(OpCode::SuperInvoke),
            38 => Some(OpCode::Import),
            39 => Some(OpCode::EndModule),
            40 => Some(OpCode::ReturnModule),
            41 => Some(OpCode::ImportVariable),
            42 => Some(OpCode::ImportVariableAs),
            43 => Some(OpCode::ImportModule),
            44 => Some(OpCode::ImportModuleAs),
            45 => Some(OpCode::ImportMethod),
            46 => Some(OpCode::ImportMethodAs),
            47 => Some(OpCode::ImportClass),
            48 => Some(OpCode::ImportClassAs),
            49 => Some(OpCode::ImportSuperclass),
            50 => Some(OpCode::ImportSuperclassAs),
            51 => Some(OpCode::ImportProperty),
            52 => Some(OpCode::ImportPropertyAs),
            53 => Some(OpCode::ImportMethodProperty),
            54 => Some(OpCode::ImportMethodPropertyAs),
            55 => Some(OpCode::ImportClassProperty),
            56 => Some(OpCode::ImportClassPropertyAs),
            57 => Some(OpCode::ImportSuperclassProperty),
            58 => Some(OpCode::ImportSuperclassPropertyAs),
            59 => Some(OpCode::String),
            _ => None,
        }
    }
}

impl std::fmt::Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Return => write!(f, "OP_RETURN"),
            OpCode::Constant => write!(f, "OP_CONSTANT"),
            OpCode::ConstantLong => write!(f, "OP_CONSTANT_LONG"),
            OpCode::Nil => write!(f, "OP_NIL"),
            OpCode::True => write!(f, "OP_TRUE"),
            OpCode::False => write!(f, "OP_FALSE"),
            OpCode::Negate => write!(f, "OP_NEGATE"),
            OpCode::Add => write!(f, "OP_ADD"),
            OpCode::Subtract => write!(f, "OP_SUBTRACT"),
            OpCode::Multiply => write!(f, "OP_MULTIPLY"),
            OpCode::Divide => write!(f, "OP_DIVIDE"),
            OpCode::Not => write!(f, "OP_NOT"),
            OpCode::Equal => write!(f, "OP_EQUAL"),
            OpCode::Greater => write!(f, "OP_GREATER"),
            OpCode::Less => write!(f, "OP_LESS"),
            OpCode::Print => write!(f, "OP_PRINT"),
            OpCode::Pop => write!(f, "OP_POP"),
            OpCode::DefineGlobal => write!(f, "OP_DEFINE_GLOBAL"),
            OpCode::GetGlobal => write!(f, "OP_GET_GLOBAL"),
            OpCode::SetGlobal => write!(f, "OP_SET_GLOBAL"),
            OpCode::GetLocal => write!(f, "OP_GET_LOCAL"),
            OpCode::SetLocal => write!(f, "OP_SET_LOCAL"),
            OpCode::Jump => write!(f, "OP_JUMP"),
            OpCode::JumpIfFalse => write!(f, "OP_JUMP_IF_FALSE"),
            OpCode::Loop => write!(f, "OP_LOOP"),
            OpCode::Call => write!(f, "OP_CALL"),
            OpCode::Closure => write!(f, "OP_CLOSURE"),
            OpCode::CloseUpvalue => write!(f, "OP_CLOSE_UPVALUE"),
            OpCode::GetUpvalue => write!(f, "OP_GET_UPVALUE"),
            OpCode::SetUpvalue => write!(f, "OP_SET_UPVALUE"),
            OpCode::Class => write!(f, "OP_CLASS"),
            OpCode::Method => write!(f, "OP_METHOD"),
            OpCode::Invoke => write!(f, "OP_INVOKE"),
            OpCode::Inherit => write!(f, "OP_INHERIT"),
            OpCode::GetProperty => write!(f, "OP_GET_PROPERTY"),
            OpCode::SetProperty => write!(f, "OP_SET_PROPERTY"),
            OpCode::GetSuper => write!(f, "OP_GET_SUPER"),
            OpCode::SuperInvoke => write!(f, "OP_SUPER_INVOKE"),
            OpCode::Import => write!(f, "OP_IMPORT"),
            OpCode::EndModule => write!(f, "OP_END_MODULE"),
            OpCode::ReturnModule => write!(f, "OP_RETURN_MODULE"),
            OpCode::ImportVariable => write!(f, "OP_IMPORT_VARIABLE"),
            OpCode::ImportVariableAs => write!(f, "OP_IMPORT_VARIABLE_AS"),
            OpCode::ImportModule => write!(f, "OP_IMPORT_MODULE"),
            OpCode::ImportModuleAs => write!(f, "OP_IMPORT_MODULE_AS"),
            OpCode::ImportMethod => write!(f, "OP_IMPORT_METHOD"),
            OpCode::ImportMethodAs => write!(f, "OP_IMPORT_METHOD_AS"),
            OpCode::ImportClass => write!(f, "OP_IMPORT_CLASS"),
            OpCode::ImportClassAs => write!(f, "OP_IMPORT_CLASS_AS"),
            OpCode::ImportSuperclass => write!(f, "OP_IMPORT_SUPERCLASS"),
            OpCode::ImportSuperclassAs => write!(f, "OP_IMPORT_SUPERCLASS_AS"),
            OpCode::ImportProperty => write!(f, "OP_IMPORT_PROPERTY"),
            OpCode::ImportPropertyAs => write!(f, "OP_IMPORT_PROPERTY_AS"),
            OpCode::ImportMethodProperty => write!(f, "OP_IMPORT_METHOD_PROPERTY"),
            OpCode::ImportMethodPropertyAs => write!(f, "OP_IMPORT_METHOD_PROPERTY_AS"),
            OpCode::ImportClassProperty => write!(f, "OP_IMPORT_CLASS_PROPERTY"),
            OpCode::ImportClassPropertyAs => write!(f, "OP_IMPORT_CLASS_PROPERTY_AS"),
            OpCode::ImportSuperclassProperty => write!(f, "OP_IMPORT_SUPERCLASS_PROPERTY"),
            OpCode::ImportSuperclassPropertyAs => write!(f, "OP_IMPORT_SUPERCLASS_PROPERTY_AS"),
            OpCode::String => write!(f, "OP_STRING"),
            _ => write!(f, "Unknown opcode"),
        }
    }
}

// OpCode constants
pub const OP_RETURN: OpCode = OpCode::Return;
pub const OP_CONSTANT: OpCode = OpCode::Constant;
pub const OP_CONSTANT_LONG: OpCode = OpCode::ConstantLong;
pub const OP_NIL: OpCode = OpCode::Nil;
pub const OP_TRUE: OpCode = OpCode::True;
pub const OP_FALSE: OpCode = OpCode::False;
pub const OP_NEGATE: OpCode = OpCode::Negate;
pub const OP_ADD: OpCode = OpCode::Add;
pub const OP_SUBTRACT: OpCode = OpCode::Subtract;
pub const OP_MULTIPLY: OpCode = OpCode::Multiply;
pub const OP_DIVIDE: OpCode = OpCode::Divide;
pub const OP_NOT: OpCode = OpCode::Not;
pub const OP_EQUAL: OpCode = OpCode::Equal;
pub const OP_GREATER: OpCode = OpCode::Greater;
pub const OP_LESS: OpCode = OpCode::Less;
pub const OP_PRINT: OpCode = OpCode::Print;
pub const OP_POP: OpCode = OpCode::Pop;
pub const OP_DEFINE_GLOBAL: OpCode = OpCode::DefineGlobal;
pub const OP_GET_GLOBAL: OpCode = OpCode::GetGlobal;
pub const OP_SET_GLOBAL: OpCode = OpCode::SetGlobal;
pub const OP_GET_LOCAL: OpCode = OpCode::GetLocal;
pub const OP_SET_LOCAL: OpCode = OpCode::SetLocal;
pub const OP_JUMP: OpCode = OpCode::Jump;
pub const OP_JUMP_IF_FALSE: OpCode = OpCode::JumpIfFalse;
pub const OP_LOOP: OpCode = OpCode::Loop;
pub const OP_CALL: OpCode = OpCode::Call;
pub const OP_CLOSURE: OpCode = OpCode::Closure;
pub const OP_CLOSE_UPVALUE: OpCode = OpCode::CloseUpvalue;
pub const OP_GET_UPVALUE: OpCode = OpCode::GetUpvalue;
pub const OP_SET_UPVALUE: OpCode = OpCode::SetUpvalue;
pub const OP_CLASS: OpCode = OpCode::Class;
pub const OP_METHOD: OpCode = OpCode::Method;
pub const OP_INVOKE: OpCode = OpCode::Invoke;
pub const OP_INHERIT: OpCode = OpCode::Inherit;
pub const OP_GET_PROPERTY: OpCode = OpCode::GetProperty;
pub const OP_SET_PROPERTY: OpCode = OpCode::SetProperty;
pub const OP_GET_SUPER: OpCode = OpCode::GetSuper;
pub const OP_SUPER_INVOKE: OpCode = OpCode::SuperInvoke;
pub const OP_IMPORT: OpCode = OpCode::Import;
pub const OP_END_MODULE: OpCode = OpCode::EndModule;
pub const OP_RETURN_MODULE: OpCode = OpCode::ReturnModule;
pub const OP_IMPORT_VARIABLE: OpCode = OpCode::ImportVariable;
pub const OP_IMPORT_VARIABLE_AS: OpCode = OpCode::ImportVariableAs;
pub const OP_IMPORT_MODULE: OpCode = OpCode::ImportModule;
pub const OP_IMPORT_MODULE_AS: OpCode = OpCode::ImportModuleAs;
pub const OP_IMPORT_METHOD: OpCode = OpCode::ImportMethod;
pub const OP_IMPORT_METHOD_AS: OpCode = OpCode::ImportMethodAs;
pub const OP_IMPORT_CLASS: OpCode = OpCode::ImportClass;
pub const OP_IMPORT_CLASS_AS: OpCode = OpCode::ImportClassAs;
pub const OP_IMPORT_SUPERCLASS: OpCode = OpCode::ImportSuperclass;
pub const OP_IMPORT_SUPERCLASS_AS: OpCode = OpCode::ImportSuperclassAs;
pub const OP_IMPORT_PROPERTY: OpCode = OpCode::ImportProperty;
pub const OP_IMPORT_PROPERTY_AS: OpCode = OpCode::ImportPropertyAs;
pub const OP_IMPORT_METHOD_PROPERTY: OpCode = OpCode::ImportMethodProperty;
pub const OP_IMPORT_METHOD_PROPERTY_AS: OpCode = OpCode::ImportMethodPropertyAs;
pub const OP_IMPORT_CLASS_PROPERTY: OpCode = OpCode::ImportClassProperty;
pub const OP_IMPORT_CLASS_PROPERTY_AS: OpCode = OpCode::ImportClassPropertyAs;
pub const OP_IMPORT_SUPERCLASS_PROPERTY: OpCode = OpCode::ImportSuperclassProperty;
pub const OP_IMPORT_SUPERCLASS_PROPERTY_AS: OpCode = OpCode::ImportSuperclassPropertyAs;
pub const OP_STRING: OpCode = OpCode::String;
