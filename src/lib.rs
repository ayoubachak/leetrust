pub mod chunk;
pub mod value;
pub mod memory;
pub mod debug;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpCode {
    Return,
    Constant,
    ConstantLong,
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
}

impl OpCode {
    pub fn from_u8(value: u8) -> Option<Self> {
        match value {
            0 => Some(OpCode::Return),
            1 => Some(OpCode::Constant),
            2 => Some(OpCode::ConstantLong),
            3 => Some(OpCode::Negate),
            4 => Some(OpCode::Add),
            5 => Some(OpCode::Subtract),
            6 => Some(OpCode::Multiply),
            7 => Some(OpCode::Divide),
            8 => Some(OpCode::Not),
            9 => Some(OpCode::Equal),
            10 => Some(OpCode::Greater),
            11 => Some(OpCode::Less),
            12 => Some(OpCode::Print),
            13 => Some(OpCode::Pop),
            14 => Some(OpCode::DefineGlobal),
            15 => Some(OpCode::GetGlobal),
            16 => Some(OpCode::SetGlobal),
            17 => Some(OpCode::GetLocal),
            18 => Some(OpCode::SetLocal),
            19 => Some(OpCode::Jump),
            20 => Some(OpCode::JumpIfFalse),
            21 => Some(OpCode::Loop),
            22 => Some(OpCode::Call),
            23 => Some(OpCode::Closure),
            24 => Some(OpCode::CloseUpvalue),
            25 => Some(OpCode::GetUpvalue),
            26 => Some(OpCode::SetUpvalue),
            27 => Some(OpCode::Class),
            28 => Some(OpCode::Method),
            29 => Some(OpCode::Invoke),
            30 => Some(OpCode::Inherit),
            31 => Some(OpCode::GetProperty),
            32 => Some(OpCode::SetProperty),
            33 => Some(OpCode::GetSuper),
            34 => Some(OpCode::SuperInvoke),
            35 => Some(OpCode::Import),
            36 => Some(OpCode::EndModule),
            37 => Some(OpCode::ReturnModule),
            38 => Some(OpCode::ImportVariable),
            39 => Some(OpCode::ImportVariableAs),
            40 => Some(OpCode::ImportModule),
            41 => Some(OpCode::ImportModuleAs),
            42 => Some(OpCode::ImportMethod),
            43 => Some(OpCode::ImportMethodAs),
            44 => Some(OpCode::ImportClass),
            45 => Some(OpCode::ImportClassAs),
            46 => Some(OpCode::ImportSuperclass),
            47 => Some(OpCode::ImportSuperclassAs),
            48 => Some(OpCode::ImportProperty),
            49 => Some(OpCode::ImportPropertyAs),
            50 => Some(OpCode::ImportMethodProperty),
            51 => Some(OpCode::ImportMethodPropertyAs),
            52 => Some(OpCode::ImportClassProperty),
            53 => Some(OpCode::ImportClassPropertyAs),
            54 => Some(OpCode::ImportSuperclassProperty),
            55 => Some(OpCode::ImportSuperclassPropertyAs),
            
            _ => None,
        }
    }

    pub fn to_u8(self) -> u8 {
        match self {
            OpCode::Return => 0,
            OpCode::Constant => 1,
            OpCode::ConstantLong => 2,
            OpCode::Negate => 3,
            OpCode::Add => 4,
            OpCode::Subtract => 5,
            OpCode::Multiply => 6,
            OpCode::Divide => 7,
            OpCode::Not => 8,
            OpCode::Equal => 9,
            OpCode::Greater => 10,
            OpCode::Less => 11,
            OpCode::Print => 12,
            OpCode::Pop => 13,
            OpCode::DefineGlobal => 14,
            OpCode::GetGlobal => 15,
            OpCode::SetGlobal => 16,
            OpCode::GetLocal => 17,
            OpCode::SetLocal => 18,
            OpCode::Jump => 19,
            OpCode::JumpIfFalse => 20,
            OpCode::Loop => 21,
            OpCode::Call => 22,
            OpCode::Closure => 23,
            OpCode::CloseUpvalue => 24,
            OpCode::GetUpvalue => 25,
            OpCode::SetUpvalue => 26,
            OpCode::Class => 27,
            OpCode::Method => 28,
            OpCode::Invoke => 29,
            OpCode::Inherit => 30,
            OpCode::GetProperty => 31,
            OpCode::SetProperty => 32,
            OpCode::GetSuper => 33,
            OpCode::SuperInvoke => 34,
            OpCode::Import => 35,
            OpCode::EndModule => 36,
            OpCode::ReturnModule => 37,
            OpCode::ImportVariable => 38,
            OpCode::ImportVariableAs => 39,
            OpCode::ImportModule => 40,
            OpCode::ImportModuleAs => 41,
            OpCode::ImportMethod => 42,
            OpCode::ImportMethodAs => 43,
            OpCode::ImportClass => 44,
            OpCode::ImportClassAs => 45,
            OpCode::ImportSuperclass => 46,
            OpCode::ImportSuperclassAs => 47,
            OpCode::ImportProperty => 48,
            OpCode::ImportPropertyAs => 49,
            OpCode::ImportMethodProperty => 50,
            OpCode::ImportMethodPropertyAs => 51,
            OpCode::ImportClassProperty => 52,
            OpCode::ImportClassPropertyAs => 53,
            OpCode::ImportSuperclassProperty => 54,
            OpCode::ImportSuperclassPropertyAs => 55,
        }
    }

    pub fn from_usize(index: usize) -> Option<Self> {
        match index {
            0 => Some(OpCode::Return),
            1 => Some(OpCode::Constant),
            2 => Some(OpCode::ConstantLong),
            3 => Some(OpCode::Negate),
            4 => Some(OpCode::Add),
            5 => Some(OpCode::Subtract),
            6 => Some(OpCode::Multiply),
            7 => Some(OpCode::Divide),
            8 => Some(OpCode::Not),
            9 => Some(OpCode::Equal),
            10 => Some(OpCode::Greater),
            11 => Some(OpCode::Less),
            12 => Some(OpCode::Print),
            13 => Some(OpCode::Pop),
            14 => Some(OpCode::DefineGlobal),
            15 => Some(OpCode::GetGlobal),
            16 => Some(OpCode::SetGlobal),
            17 => Some(OpCode::GetLocal),
            18 => Some(OpCode::SetLocal),
            19 => Some(OpCode::Jump),
            20 => Some(OpCode::JumpIfFalse),
            21 => Some(OpCode::Loop),
            22 => Some(OpCode::Call),
            23 => Some(OpCode::Closure),
            24 => Some(OpCode::CloseUpvalue),
            25 => Some(OpCode::GetUpvalue),
            26 => Some(OpCode::SetUpvalue),
            27 => Some(OpCode::Class),
            28 => Some(OpCode::Method),
            29 => Some(OpCode::Invoke),
            30 => Some(OpCode::Inherit),
            31 => Some(OpCode::GetProperty),
            32 => Some(OpCode::SetProperty),
            33 => Some(OpCode::GetSuper),
            34 => Some(OpCode::SuperInvoke),
            35 => Some(OpCode::Import),
            36 => Some(OpCode::EndModule),
            37 => Some(OpCode::ReturnModule),
            38 => Some(OpCode::ImportVariable),
            39 => Some(OpCode::ImportVariableAs),
            40 => Some(OpCode::ImportModule),
            41 => Some(OpCode::ImportModuleAs),
            42 => Some(OpCode::ImportMethod),
            43 => Some(OpCode::ImportMethodAs),
            44 => Some(OpCode::ImportClass),
            45 => Some(OpCode::ImportClassAs),
            46 => Some(OpCode::ImportSuperclass),
            47 => Some(OpCode::ImportSuperclassAs),
            48 => Some(OpCode::ImportProperty),
            49 => Some(OpCode::ImportPropertyAs),
            50 => Some(OpCode::ImportMethodProperty),
            51 => Some(OpCode::ImportMethodPropertyAs),
            52 => Some(OpCode::ImportClassProperty),
            53 => Some(OpCode::ImportClassPropertyAs),
            54 => Some(OpCode::ImportSuperclassProperty),
            55 => Some(OpCode::ImportSuperclassPropertyAs),
            _ => None,
        }
    }
}

impl std::fmt::Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Return => write!(f, "OP_RETURN"),
            OpCode::Constant => write!(f, "OP_CONSTANT"),
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
            _ => write!(f, "Unknown opcode"),
        }
    }
}

// OpCode constants
pub const OP_RETURN: OpCode = OpCode::Return;
pub const OP_CONSTANT: OpCode = OpCode::Constant;
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

