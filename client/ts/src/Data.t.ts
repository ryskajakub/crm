import { DateTime } from "luxon";

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
  warranty: boolean | null;
  noFaults: boolean | null;
};

export type ParsedValue<A> = {
  value: string;
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

export type ValidatedJob = {
  [K in Exclude<keyof Job, "note">]: ParsedValue<DateTime>;
} &
  Pick<Job, "note">;

export type Part = {
  number: string;
  name: string;
  quantity: string;
  machine_id: number | null;
};

export type Transport = "reality" | "km";

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
