import { DateTime } from "luxon";

export type Payload<T> = T extends Date
  ? string
  : T extends {}
  ? { [K in keyof T]: Payload<T[K]> }
  : T extends [infer X]
  ? [Payload<X>]
  : T;

export type Unpack<A extends any[]> = A extends (infer B)[] ? B : never;

export type FormState = {
  employees: (number | null)[];
  km: number;
  transport: Transport;
  mileages: Record<number, number>;
  jobs: ValidatedJob[];
  parts: Part[];
  description: string;
  recommendation: string;
  warranty: ParsedValue<boolean | null, boolean>;
  noFaults: ParsedValue<boolean | null, boolean>;
};

export type ParsedForm = Omit<
  FormState,
  "jobs" | "warranty" | "noFaults" | "employees"
> & {
  employees: number[];
  jobs: SubmitJob[];
  warranty: boolean;
  noFaults: boolean;
};

export type ParsedValue<Value, A> = {
  value: Value;
  result: { type: "error"; error: string } | { type: "ok"; value: A };
  displayError: boolean;
};

export type Job = {
  date: string;
  travelThereFrom: string;
  travelThereTo: string;
  workFrom: string;
  workTo: string;
  travelBackFrom: string;
  travelBackTo: string;
  note: string;
};

export type TypedJob<T> = {
  [K in Exclude<keyof Job, "note">]: T;
} &
  Pick<Job, "note">;

export type ValidatedJob = TypedJob<ParsedValue<string, DateTime>>;
export type SubmitJob = TypedJob<Date>;

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
  date: Date;
};

export type AppPropsDataClient = {
  type: "client";
};

export type AppPropsDataServer = {
  type: "server";
  parsedForm: ParsedForm;
  signatures: Signatures;
};

export type AppPropsData = AppPropsDataServer | AppPropsDataClient;

export type AppProps = { data: AppPropsData; signature: boolean };
