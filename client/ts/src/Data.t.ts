export type Unpack<A extends any[]> = A extends (infer B)[] ? B : never;

export type FormState = {
  employees: (number | null)[];
  km: number;
  transport: Transport;
  mileages: Record<number, number>;
  jobs: Job[];
  parts: Part[];
  description: string;
  recommendation: string;
  warranty: boolean | null;
  noFaults: boolean | null;
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