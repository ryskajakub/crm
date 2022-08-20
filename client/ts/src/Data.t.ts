import { DateTime } from "luxon";

export type ParsedValue<Value, A> = {
  value: Value;
  result: { type: "error"; error: string } | { type: "ok"; value: A };
  displayError: boolean;
};

export type Payload<T> = null extends T
  ? Payload<Exclude<T, null>> | null
  : T extends DateTime
  ? string
  : T extends Time
  ? string
  : T extends Radio
  ? boolean
  : T extends IdItem
  ? number
  : T extends {}
  ? { [K in keyof T]: Payload<T[K]> }
  : T extends [infer X]
  ? [Payload<X>]
  : T;

export type XXX = Payload<{ item: Date }>;

export type Form<T> = null extends T
  ? Form<Exclude<T, null>> | null
  : T extends DateTime
  ? ParsedValue<string, DateTime>
  : T extends Time
  ? ParsedValue<string, DateTime>
  : T extends Radio
  ? ParsedValue<boolean | null, boolean>
  : T extends IdItem
  ? null | number
  : T extends {}
  ? { [K in keyof T]: Form<T[K]> }
  : T extends [infer X]
  ? [Form<X>]
  : T;

export type Db<T> = null extends T
  ? Db<Exclude<T, null>> | null
  : T extends DateTime
  ? string
  : T extends Time
  ? string
  : T extends Radio
  ? boolean
  : T extends IdItem
  ? number
  : T extends {}
  ? { [K in keyof T]: Db<T[K]> }
  : T extends [infer X]
  ? [Db<X>]
  : T;

export type Ts<T> = null extends T
  ? Ts<Exclude<T, null>> | null
  : T extends DateTime
  ? DateTime
  : T extends Time
  ? DateTime
  : T extends Radio
  ? boolean
  : T extends IdItem
  ? number
  : T extends {}
  ? { [K in keyof T]: Ts<T[K]> }
  : T extends [infer X]
  ? [Ts<X>]
  : T;

export type Unpack<A extends any[]> = A extends (infer B)[] ? B : never;

export class Time {
  private __time: null = null;
}

export class Radio {
  private __radio: null = null;
}

export class IdItem {
  private __idItem: null = null;
}

export type FormState = {
  employees: IdItem[];
  km: number;
  transport: Transport;
  mileages: Record<number, number>;
  jobs: Job[];
  parts: Part[];
  description: string;
  recommendation: string;
  warranty: Radio;
  noFaults: Radio;
};

export type Job = {
  date: DateTime;
  travelThereFrom: Time;
  travelThereTo: Time;
  workFrom: Time;
  workTo: Time;
  travelBackFrom: Time;
  travelBackTo: Time;
  note: string;
};

export type Part = {
  number: string;
  name: string;
  quantity: string;
  machine_id: number | null;
};

export type Transport = "reality" | "km";

export type Signatures = {
  ours: string;
  theirs: string;
};

export type Upkeep = {
  company_name: string;
  employees: number[];
  available_employees: {
    id: number;
    name: string;
  }[];
  machines: {
    serial_number: string;
    mileage: number;
    type: string;
    manufacturer: string;
    machine_id: number;
  }[];
  date: DateTime;
  description: string;
  recommendation: string;
};

export type ServerForm = Omit<
  FormState,
  "employees" | "mileages" | "description" | "recommendation"
>;

export type DownloadData = {
  upkeep: Upkeep;
  form: ServerForm | null;
  signatures: Signatures | null;
};

export type AppPropsDataClient = {
  type: "client";
};

export type AppPropsDataServer = {
  type: "server";
  parsedForm: Payload<ServerForm>;
  signatures: Ts<Signatures>;
  upkeep: Ts<Upkeep>,
  upkeepId: number,
};

export type AppPropsData = AppPropsDataServer | AppPropsDataClient;

export type AppProps = { data: AppPropsData; signature: boolean };
