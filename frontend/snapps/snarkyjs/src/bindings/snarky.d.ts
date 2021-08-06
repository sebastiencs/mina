export type JSONValue = number | string | boolean | null | Array<JSON> | { [key: string]: JSONValue };

export class Keypair {
}

export class Proof {
}

export type AsField = Field | number | string | boolean;

export class Field {
  constructor(x: Field | number | string | boolean);

  neg(): Field;
  inv(): Field;

  add(y: Field | number | string | boolean): Field;
  sub(y: Field | number | string | boolean): Field;
  mul(y: Field | number | string | boolean): Field;
  div(y: Field | number | string | boolean): Field;

  square(): Field;
  sqrt(): Field;

  toString(): string;

  sizeInFieldElements(): number;
  toFieldElements(): Field[];

  lt(y: Field | number | string | boolean): Bool;
  lte(y: Field | number | string | boolean): Bool;
  gt(y: Field | number | string | boolean): Bool;
  gte(y: Field | number | string | boolean): Bool;

  assertLt(y: Field | number | string | boolean): void;
  assertLte(y: Field | number | string | boolean): void;
  assertGt(y: Field | number | string | boolean): void;
  assertGte(y: Field | number | string | boolean): void;

  assertEquals(y: Field | number | string | boolean): void;
  assertBoolean(): void;
  isZero(): Bool;

  toBits(): Bool[];

  equals(y: Field | number | string | boolean): Bool;

  // value(this: Field | number | string | boolean): Field;

  /* Self members */
  static one: Field;
  static zero: Field;
  static random(): Field;

  static neg(x: Field | number | string | boolean): Field;
  static inv(x: Field | number | string | boolean): Field;

  static add(x: Field | number | string | boolean, y: Field | number | string | boolean): Field;
  static sub(x: Field | number | string | boolean, y: Field | number | string | boolean): Field;
  static mul(x: Field | number | string | boolean, y: Field | number | string | boolean): Field;
  static div(x: Field | number | string | boolean, y: Field | number | string | boolean): Field;

  static square(x: Field | number | string | boolean): Field;
  static sqrt(x: Field | number | string | boolean): Field;

  static toString(x: Field | number | string | boolean): string;

  static sizeInFieldElements(): number;
  static toFieldElements(x: Field | number | string | boolean): Field[];
  static ofFieldElements(fields: Field[]): Field;

  static assertEqual(x: Field | number | string | boolean, y: Field | number | string | boolean): Field;
  static assertBoolean(x: Field | number | string | boolean): void;
  static isZero(x: Field | number | string | boolean): Bool;

  static ofBits(x: Bool | boolean[]): Field;
  static toBits(x: Field | number | string | boolean): Bool[];

  static equal(x: Field | number | string | boolean, y: Field | number | string | boolean): Bool;

  static toJSON(x: Field): JSONValue;
  static fromJSON(x: JSONValue): Field | null;
}

export class Bool {
  constructor(x: Bool | boolean);

  toField(): Field;

  not(): Bool;
  and(y: Bool | boolean): Bool;
  or(y: Bool | boolean): Bool;

  assertEquals(y: Bool | boolean): void;

  equals(y: Bool | boolean): Bool;
  isTrue(): Bool;
  isFalse(): Bool;

  sizeInFieldElements(): number;
  toFieldElements(): Field[];

  toString(): string;

  /* Can only be called on non-witness values */
  toBoolean(): boolean;

  /* static members */
  static toField(x: Bool | boolean): Field;

  static Unsafe: {
    ofField(x: Field | number | string | boolean): Bool;
  };

  static not(x: Bool | boolean): Bool;
  static and(x: Bool | boolean, y: Bool | boolean): Bool;
  static or(x: Bool | boolean, y: Bool | boolean): Bool;

  static assertEqual(x: Bool | boolean, y: Bool | boolean): void;

  static equal(x: Bool | boolean, y: Bool | boolean): Bool;
  static isTrue(x: Bool | boolean): Bool;
  static isFalse(x: Bool | boolean): Bool;

  static count(x: Bool | boolean[]): Field;

  static sizeInFieldElements(): number;
  static toFieldElements(x: Bool | boolean): Field[];
  static ofFieldElements(fields: Field[]): Bool;

  static toJSON(x: Bool): JSONValue;
  static fromJSON(x: JSONValue): Bool | null;
}

export interface AsFieldElements<T> {
  toFieldElements(x: T): Field[];
  ofFieldElements(x: Field[]): T;
  sizeInFieldElements(): number;
}

export interface CircuitMain<W, P> {
  snarkyWitnessTyp: AsFieldElements<W>,
  snarkyPublicTyp: AsFieldElements<P>,
  snarkyMain: (W, P) => void
}

export class Circuit {
  static addConstraint(
    this: Circuit,
    kind: 'multiply',
    x: Field,
    y: Field,
    z: Field
  ): void;
  static addConstraint(this: Circuit, kind: 'add', x: Field, y: Field, z: Field): void;
  static addConstraint(
    this: Circuit,
    kind: 'equal',
    x: Field,
    y: Field,
    z: Field
  ): void;
  static addConstraint(
    this: Circuit,
    kind: 'boolean',
    x: Field,
    y: Field,
    z: Field
  ): void;

  static newVariable(f: () => Field | number | string | boolean): Field;

  static witness<T>(
    ctor: { toFieldElements(x: T): Field[]; ofFieldElements(x: Field[]): T; sizeInFieldElements(): number },
    f: () => T
  ): T;

  static array<T>(
    ctor: AsFieldElements<T>,
    length: number
  ): AsFieldElements<T[]>;

  static assertEqual<T>(
    ctor: { toFieldElements(x: T): Field[] },
    x: T,
    y: T
  ): void;

  static assertEqual(
    x: T,
    y: T
  ): void;

  static equal<T>(
    ctor: { toFieldElements(x: T): Field[] },
    x: T,
    y: T
  ): Bool;

  static equal(
    x: T,
    y: T
  ): Bool;

  static if<T>(
    b: Bool | boolean,
    ctor: AsFieldElements<T>,
    x: T,
    y: T
  ): T;

  static if(
    b: Bool | boolean,
    x: T,
    y: T
  ): T;

  static generateKeypair(): Keypair;

  static prove(
    w: any[], p: any[],
    kp: Keypair
  ): Proof;
}

export class Scalar {
    toFieldElements(this: Scalar): Field[];

    /* Can only be called on non-witness values */
    neg(): Scalar;
    add(y: Scalar): Scalar;
    sub(y: Scalar): Scalar;
    mul(y: Scalar): Scalar;
    div(y: Scalar): Scalar;

    static toFieldElements(x: Scalar): Field[]
    static ofFieldElements(fields: Field[]): Scalar;
    static sizeInFieldElements(): number;
    static ofBits(bits: Bool[]): Scalar;
    static random(): Scalar;

    static toJSON(x: Scalar): JSONValue;
    static fromJSON(x: JSONValue): Scalar | null;
}

export class EndoScalar {
    static toFieldElements(x: Scalar): Field[]
    static ofFieldElements(fields: Field[]): Scalar;
    static sizeInFieldElements(): number;
}

export class Group {
    x: Field;
    y: Field;

    add(y: Group): Group;
    sub(y: Group): Group;
    neg(): Group;
    scale(y: Scalar): Group;
    endoScale(y: EndoScalar): Group;

    assertEquals(y: Group): void;
    equals(y: Group): Bool;

    constructor(args: { x: Field | number | string | boolean, y: Field | number | string | boolean })
    constructor(x: Field | number | string | boolean, y: Field | number | string | boolean)

    static generator: Group;
    static add(x: Group, y: Group): Group;
    static sub(x: Group, y: Group): Group;
    static neg(x: Group): Group;
    static scale(x: Group, y: Scalar): Group;
    static endoScale(x: Group, y: EndoScalar): Group;

    static assertEqual(x: Group, y: Group): void;
    static equal(x: Group, y: Group): Bool;

    static toFieldElements(x: Group): Field[]
    static ofFieldElements(fields: Field[]): Group;
    static sizeInFieldElements(): number;

    static toJSON(x: Group): JSONValue;
    static fromJSON(x: JSONValue): Group | null;
}

export const Poseidon : {
  hash: (xs: Field[]) => Field;
};

/* TODO: Figure out types for these. */
export const ofFieldElements: (x: any[], y: any[]) => any[];
export const toFieldElements: (x: any[], y: any[]) => any[];
export const sizeInFieldElements: (x: any[]) => number;

export const NumberAsField: AsFieldElements<Number>;

export const array: <T>(x: AsFieldElements<T>, length: number) => AsFieldElements<T[]>;
