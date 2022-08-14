import "bootstrap/dist/css/bootstrap.css";

import SignatureCanvas from "react-signature-canvas";
import { useEffect, useReducer, useRef, useState } from "react";
import axios from "axios";
import { DateTime } from "luxon";

import h from "react-hyperscript";
import hh from "hyperscript-helpers";

// @ts-ignore
import svgs from "hyperscript-helpers/dist/svg";

const {
  div,
  strong,
  input,
  select,
  option,
  button,
  svg,
  label,
  p,
  span,
  textarea,
  img,
  form,
} = hh(h);
/** @type {{ path: import("hyperscript-helpers").HyperScriptHelperFn }} */
const { path } = svgs(h);

/**
 * @typedef { { type: "initial" } } Initial
 * @typedef { { type: "success", upkeepId: number, upkeep: import("./Data.t").Upkeep, formState: import("./Data.t").FormState } } Success
 * @typedef { { type: "failure" } } Failure
 * @typedef { Initial | Success | Failure } State
 *
 * @typedef { { type: "set_employees", employees: (number | null)[] } } SetEmployees
 * @typedef { { type: "set_km", km: number } } SetKm
 * @typedef { { type: "set_transport", transport: import("./Data.t").Transport } } SetTransport
 * @typedef { { type: "set_failure" } } SetFailure
 * @typedef { { type: "set_success", upkeepId: number, upkeep: import("./Data.t").Upkeep, formState: import("./Data.t").FormState } } SetSuccess
 * @typedef { { type: "set_mileage", machine_id: number, mileage: number } } SetMileage
 * @typedef { { type: "set_job", index: number, field: keyof import("./Data.t").Job, value: string, result: import("./Data.t").ParsedValue<string, DateTime>["result"] } } SetJob
 * @typedef { { type: "set_job_note", index: number, value: string } } SetJobNote
 * @typedef { { type: "set_part", index: number, field: Exclude<keyof import("./Data.t").Part, "machine_id">, value: string } } SetPart
 * @typedef { { type: "set_part_link", index: number, machine_id: number | null } } SetPartLink
 * @typedef { { type: "set_string", field: Extract<keyof import("./Data.t").FormState, "recommendation" | "description">, value: string } } SetString
 * @typedef { { type: "set_boolean", field: Extract<keyof import("./Data.t").FormState, "warranty" | "noFaults">, value: import("./Data.t").ParsedValue<null | boolean, boolean> } } SetBoolean
 * @typedef { { type: "remove_at_index", field: Extract<keyof import("./Data.t").FormState, "jobs" | "parts" | "employees">, index: number } } RemoveAtIndex
 * @typedef { { type: "display_errors" } } DisplayErrors
 * @typedef { { type: "add_item", field: Extract<keyof import("./Data.t").FormState, "jobs" | "parts" | "employees"> } } AddItem
 * @typedef { SetEmployees | SetKm | SetTransport | SetFailure | SetSuccess | SetMileage | SetJob | SetPart | SetPartLink | SetString | SetBoolean | RemoveAtIndex | AddItem | DisplayErrors | SetJobNote } Action
 * @typedef {Extract<keyof import("./Data.t").Job, "travelThereFrom" | "travelThereTo" | "workFrom" | "workTo" | "travelBackFrom" | "travelBackTo">} TimeKey
 */

/** @type {(condition: boolean, element: React.ReactElement) => React.ReactElement} */
const when = (condition, element) => {
  if (condition) {
    return element;
  } else {
    return h(React.Fragment);
  }
};

/** @type { readonly TimeKey[] } */
const timeKeys = /** @type {const} */ ([
  "travelThereFrom",
  "travelThereTo",
  "workFrom",
  "workTo",
  "travelBackFrom",
  "travelBackTo",
]);

/** @type { readonly (Extract<keyof import("./Data.t").Job, "date"> | TimeKey)[] } */
const validatedJobKeys = [...timeKeys, "date"];

/**
 * @template {Extract<keyof import("./Data.t").FormState, "jobs" | "parts" | "employees">} T
 * @param {T} field
 * @returns { import("./Data.t").Unpack<import("./Data.t").FormState[T]> }
 */
function newItem(field) {
  /** @type {import("./Data.t").ParsedValue<string, DateTime>} */
  const newValidatedField = {
    value: "",
    displayError: false,
    result: {
      type: "error",
      error: "Nevyplněno",
    },
  };

  switch (field) {
    case "employees":
      // @ts-ignore
      return null;
    case "jobs":
      // @ts-ignore
      return {
        date: newValidatedField,
        travelThereFrom: newValidatedField,
        travelThereTo: newValidatedField,
        workFrom: newValidatedField,
        workTo: newValidatedField,
        travelBackFrom: newValidatedField,
        travelBackTo: newValidatedField,
        note: "",
      };
    case "parts":
      // @ts-ignore
      return {
        number: "",
        name: "",
        quantity: "",
        machine_id: null,
      };
  }
  // @ts-ignore
  return null;
}

/**
 * @template T
 * @param {Array<T>} array
 * @param {number} index
 * @param {T} element
 * @returns { Array<T> }
 */
function replaceAtIndex(array, index, element) {
  return [...array.slice(0, index), element, ...array.slice(index + 1)];
}

/**
 * @template T
 * @param {Array<T>} array
 * @param {number} index
 * @returns { Array<T> }
 */
function removeAtIndex(array, index) {
  return [...array.slice(0, index), ...array.slice(index + 1)];
}

/**
 * @param { boolean } signature
 * @param { import("./Data.t").Part[] } parts
 * @param { Pick<import("./Data.t").Unpack<import("./Data.t").Upkeep["machines"]>, "machine_id" | "serial_number" | "manufacturer">[] } machines
 * @param { React.Dispatch<Action> } dispatch
 * @returns { React.ReactElement[] }
 */
function renderParts(signature, parts, machines, dispatch) {
  const partsRendered = parts.map((part, index) => {
    /** @type { (field: SetPart["field"]) => import("react").ChangeEventHandler<HTMLInputElement> } */
    const onChange = (field) => (e) => {
      const value = e.target.value;
      /** @type { SetPart } */
      const setPart = {
        type: "set_part",
        field,
        index,
        value,
      };
      dispatch(setPart);
    };
    return div({ key: index, className: "row" }, [
      div({ className: "col-lg-1" }, index + 1),
      div(
        { className: "col-lg" },
        input({
          type: "text",
          className: "form-control",
          value: parts[index].number,
          onChange: onChange("number"),
        })
      ),
      div(
        { className: "col-lg" },
        input({
          type: "text",
          className: "form-control",
          value: parts[index].name,
          onChange: onChange("name"),
        })
      ),
      div(
        { className: "col-lg-1" },
        input({
          type: "text",
          className: "form-control",
          value: parts[index].quantity,
          onChange: onChange("quantity"),
        })
      ),
      div(
        { className: "col-lg-2" },
        select(
          {
            value: part.machine_id || "---",
            onChange: (e) => {
              const value = e.target.value;
              const valueParsed = value === "---" ? null : Number(value);
              /** @type { SetPartLink } */
              const setParkLink = {
                type: "set_part_link",
                index,
                machine_id: valueParsed,
              };
              dispatch(setParkLink);
            },
            className: "form-select",
          },
          [
            option({ key: null, value: "---" }, "---"),
            ...[
              machines.map((m) => {
                return option(
                  { key: m.machine_id, value: m.machine_id },
                  `${m.manufacturer} ${m.serial_number}`
                );
              }),
            ],
          ]
        )
      ),
      div(
        { className: "col-lg-1" },
        signature
          ? ""
          : button(
              {
                className: "btn btn-danger",
                type: "button",
                onClick: () =>
                  dispatch({ type: "remove_at_index", field: "parts", index }),
              },
              svg(
                {
                  xmlns: "http://www.w3.org/2000/svg",
                  width: "16",
                  height: "16",
                  fill: "currentColor",
                  className: "bi bi-dash-lg",
                  viewBox: "0 0 16 16",
                },
                path({
                  fillRule: "evenodd",
                  d:
                    "M2 8a.5.5 0 0 1 .5-.5h11a.5.5 0 0 1 0 1h-11A.5.5 0 0 1 2 8Z",
                })
              )
            )
      ),
    ]);
  });
  const partsRenderedRows = [
    ...partsRendered,
    signature
      ? ""
      : div(
          { className: "row" },
          div(
            {
              className: "col-lg-12",
            },
            button(
              {
                type: "button",
                className: "btn btn-primary",
                onClick: () => {
                  dispatch({ type: "add_item", field: "parts" });
                },
              },
              svg(
                {
                  xmlns: "http://www.w3.org/2000/svg",
                  width: "16",
                  height: "16",
                  fill: "currentColor",
                  className: "bi bi-plus",
                  viewBox: "0 0 16 16",
                },
                path({
                  d:
                    "M8 4a.5.5 0 0 1 .5.5v3h3a.5.5 0 0 1 0 1h-3v3a.5.5 0 0 1-1 0v-3h-3a.5.5 0 0 1 0-1h3v-3A.5.5 0 0 1 8 4z",
                })
              )
            )
          )
        ),
  ];

  return [
    div({ className: "row" }, [
      div({ className: "col-lg-1" }, strong("Pořadí")),
      div({ className: "col-lg" }, strong("Číslo materiálu")),
      div({ className: "col-lg" }, strong("Název materiálu")),
      div({ className: "col-lg-1" }, strong("Množství")),
      div({ className: "col-lg-2" }, strong("Zařízení")),
      div({ className: "col-lg-1" }),
    ]),
    ...partsRenderedRows,
  ];
}

/**
 * @param { boolean } signature
 * @param { import("./Data.t").ValidatedJob[] } jobs
 * @param { React.Dispatch<Action> } dispatch
 * @returns { React.ReactElement[] }
 */
function renderJobs(signature, jobs, dispatch) {
  const jobsRendered = jobs.map((job, index) => {
    /** @type { (field: keyof import("./Data.t").Job, parse: (value: string) => import("./Data.t").ParsedValue<string, DateTime>["result"]) => import("react").ChangeEventHandler<HTMLInputElement> } */
    const onChange = (field, parse) => (e) => {
      const value = e.target.value;

      const result = parse(value);

      /** @type { SetJob } */
      const setJob = {
        type: "set_job",
        field,
        index,
        value,
        result,
      };
      dispatch(setJob);
    };

    /** @type { (inputValue: string) => import("./Data.t").ParsedValue<string, DateTime>["result"] } */
    const parseDate = (inputValue) => {
      const value = DateTime.fromFormat(inputValue, "d.L.y");

      /** @type { import("./Data.t").ParsedValue<string, DateTime>["result"] } */
      return value.isValid
        ? { type: "ok", value }
        : { type: "error", error: "špatné datum" };
    };

    /** @type { (inputValue: string) => import("./Data.t").ParsedValue<string, DateTime>["result"] } */
    const parseTime = (inputValue) => {
      const value = DateTime.fromFormat(inputValue, "H:m");

      if (value.isValid) {
        return { type: "ok", value };
      } else {
        const value2 = DateTime.fromFormat(inputValue, "H");

        /** @type { import("./Data.t").ParsedValue<string, DateTime>["result"] } */
        return value2.isValid
          ? { type: "ok", value: value2 }
          : { type: "error", error: "špatný čas" };
      }
    };

    return div({ key: index, className: "row" }, [
      div({ className: "col-lg-2" }, [
        input({
          type: "text",
          className: "form-control",
          onChange: onChange("date", parseDate),
          value: job.date.value,
          id: `date${index}`,
        }),
        job.date.displayError && job.date.result.type === "error"
          ? label(
              { htmlFor: `date${index}`, className: "text-danger" },
              job.date.result.error
            )
          : "",
      ]),
      ...timeKeys.map((timeKey) => {
        const result = job[timeKey].result;
        return div({ key: timeKey, className: "col-lg-1" }, [
          input({
            type: "text",
            className: "form-control",
            onChange: onChange(timeKey, parseTime),
            value: job[timeKey].value,
            id: `${timeKey}${index}`,
          }),
          job[timeKey].displayError && result.type === "error"
            ? label(
                { htmlFor: `${timeKey}${index}`, className: "text-danger" },
                result.error
              )
            : "",
        ]);
      }),
      div(
        { className: "col-lg" },
        input({
          type: "text",
          className: "form-control",
          onChange: (e) => {
            /** @type { SetJobNote } */
            const setJobNote = {
              type: "set_job_note",
              index,
              value: e.target.value,
            };
            dispatch(setJobNote);
          },
          value: job.note,
        })
      ),
      div(
        { className: "col-lg-1" },
        when(
          index !== 0 && !signature,
          button(
            {
              className: "btn btn-danger",
              type: "button",
              onClick: () =>
                dispatch({ type: "remove_at_index", field: "jobs", index }),
            },
            svg(
              {
                xmlns: "http://www.w3.org/2000/svg",
                width: "16",
                height: "16",
                fill: "currentColor",
                className: "bi bi-dash-lg",
                viewBox: "0 0 16 16",
              },
              path({
                fillRule: "evenodd",
                d:
                  "M2 8a.5.5 0 0 1 .5-.5h11a.5.5 0 0 1 0 1h-11A.5.5 0 0 1 2 8Z",
              })
            )
          )
        )
      ),
    ]);
  });

  const renderedJobRows = [
    ...jobsRendered,
    !signature
      ? ""
      : div(
          { className: "row" },
          div(
            { className: "col-lg-12" },
            button(
              {
                className: "btn btn-danger",
                type: "button",
                onClick: () => {
                  dispatch({ type: "add_item", field: "jobs" });
                },
              },
              svg(
                {
                  xmlns: "http://www.w3.org/2000/svg",
                  width: "16",
                  height: "16",
                  fill: "currentColor",
                  className: "bi bi-calendar-plus",
                  viewBox: "0 0 16 16",
                },
                [
                  path({
                    d:
                      "M8 7a.5.5 0 0 1 .5.5V9H10a.5.5 0 0 1 0 1H8.5v1.5a.5.5 0 0 1-1 0V10H6a.5.5 0 0 1 0-1h1.5V7.5A.5.5 0 0 1 8 7z",
                  }),
                  path({
                    d:
                      "M3.5 0a.5.5 0 0 1 .5.5V1h8V.5a.5.5 0 0 1 1 0V1h1a2 2 0 0 1 2 2v11a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2V3a2 2 0 0 1 2-2h1V.5a.5.5 0 0 1 .5-.5zM1 4v10a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1V4H1z",
                  }),
                ]
              )
            )
          )
        ),
  ];

  return [
    div({ className: "row" }, [
      div({ className: "col-lg-1" }, strong("Den, rok")),
      div({ className: "col-lg" }, strong("Cesta tam")),
      div({ className: "col-lg" }, strong("Práce")),
      div({ className: "col-lg" }, strong("Cesta zpět")),
      div({ className: "col-lg-2" }, strong("Poznámky")),
    ]),
    div({ className: "row" }, [
      div({ className: "col-lg-2" }, "Datum"),
      div({ className: "col-lg-1" }, "od"),
      div({ className: "col-lg-1" }, "do"),
      div({ className: "col-lg-1" }, "od"),
      div({ className: "col-lg-1" }, "do"),
      div({ className: "col-lg-1" }, "od"),
      div({ className: "col-lg-1" }, "do"),
      div({ className: "col-lg" }),
      div({ className: "col-lg-1" }),
    ]),
    ...renderedJobRows,
  ];
}

/**
 * @param { boolean } signature
 * @param { import("./Data.t").Upkeep["machines"] } machines
 * @param { React.Dispatch<Action> } dispatch
 * @param { Record<number, number> } mileages
 * @returns { React.ReactElement[] }
 */
function renderMachines(signature, machines, mileages, dispatch) {
  const renderedMachines = machines.map((machine) => {
    return div({ key: machine.machine_id, className: "row" }, [
      div(
        { className: "col-lg-4" },
        `Typ zařízení: ${machine.manufacturer} ${machine.type}`
      ),
      div({ className: "col-lg-4" }, `v. č. ${machine.serial_number}`),
      div(
        { className: "col-lg-2" },
        input({
          type: "number",
          className: "form-control",
          value: mileages[machine.machine_id],
          onChange: (e) => {
            const value = Number(e.target.value);
            dispatch({
              type: "set_mileage",
              machine_id: machine.machine_id,
              mileage: value,
            });
          },
        })
      ),
    ]);
  });

  return [
    div({ className: "row" }, [
      div({ className: "col-lg-4" }, strong("Typ zařízení")),
      div({ className: "col-lg-4" }, strong("Výrobní číslo")),
      div({ className: "col-lg-3" }, strong("Počet mth")),
    ]),
    ...renderedMachines,
  ];
}

/**
 * @param { boolean } signature
 * @param { (number | null)[] } employees
 * @param { import("./Data.t").Upkeep["available_employees"] } available_employees
 * @param { React.Dispatch<Action> } dispatch
 * @returns { React.ReactElement }
 */
function renderEmployees(signature, employees, available_employees, dispatch) {
  /** @type { (newEmployeesState: (number | null)[]) => void } */
  const setEmployees = (newEmployeesState) => {
    dispatch({ type: "set_employees", employees: newEmployeesState });
  };

  /** @type { React.ReactElement[] } */
  const renderedEmployees = employees.map((employee, index) => {
    /** @type { import("react").ChangeEventHandler<HTMLSelectElement> } */
    const change = (e) => {
      const value = e.target.value;
      const eId = value === "---" ? null : Number(value);
      const newEmployeesState = [
        ...employees.slice(0, index),
        eId,
        ...employees.slice(index + 1),
      ];
      setEmployees(newEmployeesState);
    };

    return div(
      {
        className: "col-lg-2",
        key: index,
      },
      div({ className: "input-group" }, [
        select(
          {
            value: employee || "---",
            onChange: change,
            className: "form-select",
          },
          [
            option({ key: null, value: "---" }, "---"),
            ...available_employees.map((a) => {
              return option({ key: a.id, value: a.id }, a.name);
            }),
          ]
        ),
        when(
          !signature,
          button(
            {
              className: "btn btn-danger",
              type: "button",
              onClick: () => {
                dispatch({
                  type: "remove_at_index",
                  field: "employees",
                  index,
                });
              },
            },
            svg(
              {
                xmlns: "http://www.w3.org/2000/svg",
                width: "16",
                height: "16",
                fill: "currentColor",
                className: "bi bi-dash-lg",
                viewBox: "0 0 16 16",
              },
              path({
                fillRule: "evenodd",
                d:
                  "M2 8a.5.5 0 0 1 .5-.5h11a.5.5 0 0 1 0 1h-11A.5.5 0 0 1 2 8Z",
              })
            )
          )
        ),
      ])
    );
  });

  const mkNewEmployee = () =>
    div(
      { className: "col-lg-2" },
      button(
        {
          type: "button",
          className: "btn btn-primary",
          onClick: () => dispatch({ type: "add_item", field: "employees" }),
        },
        svg(
          {
            xmlns: "http://www.w3.org/2000/svg",
            width: "16",
            height: "16",
            fill: "currentColor",
            className: "bi bi-person-plus",
            viewBox: "0 0 16 16",
          },
          [
            path({
              d:
                "M6 8a3 3 0 1 0 0-6 3 3 0 0 0 0 6zm2-3a2 2 0 1 1-4 0 2 2 0 0 1 4 0zm4 8c0 1-1 1-1 1H1s-1 0-1-1 1-4 6-4 6 3 6 4zm-1-.004c-.001-.246-.154-.986-.832-1.664C9.516 10.68 8.289 10 6 10c-2.29 0-3.516.68-4.168 1.332-.678.678-.83 1.418-.832 1.664h10z",
            }),
            path({
              fillRule: "evenodd",
              d:
                "M13.5 5a.5.5 0 0 1 .5.5V7h1.5a.5.5 0 0 1 0 1H14v1.5a.5.5 0 0 1-1 0V8h-1.5a.5.5 0 0 1 0-1H13V5.5a.5.5 0 0 1 .5-.5z",
            }),
          ]
        )
      )
    );

  return div({ className: "row" }, [
    div({ className: "col-lg-2" }, strong("Jméno technika:")),
    ...renderedEmployees,
    when(!signature, mkNewEmployee()),
  ]);
}

/**
 * @type { React.FC<import("./Data.t").AppProps> }
 */
export const App = (appProps) => {
  const { signature } = appProps;

  /** @type {(state: State, action: Action) => State} */
  const reducer = (state, action) => {
    /** @type {(f: (success: Success) => State) => State } */
    const ifSuccess = (f) => {
      if (state.type === "success") {
        return f(state);
      } else {
        return state;
      }
    };

    /** @type { (partial: ((state: Success) => Partial<import("./Data.t").FormState>) | Partial<import("./Data.t").FormState>) => State } */
    const setPartialState = (partial) => {
      return ifSuccess((state) => {
        return {
          ...state,
          formState: {
            ...state.formState,
            ...(partial instanceof Function ? partial(state) : partial),
          },
        };
      });
    };

    switch (action.type) {
      case "set_failure":
        return { type: "failure" };
      case "set_success":
        return {
          type: "success",
          upkeepId: action.upkeepId,
          formState: action.formState,
          upkeep: action.upkeep,
        };
      case "set_employees":
        return setPartialState({ employees: action.employees });
      case "set_km":
        return setPartialState({ km: action.km });
      case "set_transport":
        return setPartialState({ transport: action.transport });
      case "set_mileage":
        return setPartialState((success) => ({
          mileages: {
            ...success.formState.mileages,
            [action.machine_id]: action.mileage,
          },
        }));
      case "set_job":
        return setPartialState((success) => ({
          jobs: replaceAtIndex(success.formState.jobs, action.index, {
            ...success.formState.jobs[action.index],
            [action.field]: {
              ...success.formState.jobs[action.index],
              result: action.result,
              value: action.value,
            },
          }),
        }));
      case "set_job_note":
        return setPartialState((success) => ({
          jobs: replaceAtIndex(success.formState.jobs, action.index, {
            ...success.formState.jobs[action.index],
            note: action.value,
          }),
        }));
      case "set_part":
        return setPartialState((success) => ({
          parts: replaceAtIndex(success.formState.parts, action.index, {
            ...success.formState.parts[action.index],
            [action.field]: action.value,
          }),
        }));
      case "set_part_link":
        return setPartialState((success) => ({
          parts: replaceAtIndex(success.formState.parts, action.index, {
            ...success.formState.parts[action.index],
            machine_id: action.machine_id,
          }),
        }));
      case "set_string":
        return setPartialState({ [action.field]: action.value });
      case "set_boolean":
        return setPartialState({ [action.field]: action.value });
      case "add_item":
        return setPartialState((state) => ({
          [action.field]: [
            ...state.formState[action.field],
            newItem(action.field),
          ],
        }));
      case "remove_at_index":
        return setPartialState((state) => ({
          [action.field]: removeAtIndex(
            // @ts-ignore
            state.formState[action.field],
            action.index
          ),
        }));
      case "display_errors":
        /** @type { <B, A>(validatedValue: import("./Data.t").ParsedValue<B, A> ) => import("./Data.t").ParsedValue<B, A> } */
        const displayError = (validatedValue) => ({
          ...validatedValue,
          displayError: true,
        });
        return setPartialState((state) => ({
          jobs: state.formState.jobs.map((job) => ({
            ...job,
            date: displayError(job.date),
            travelThereFrom: displayError(job.travelThereFrom),
            travelThereTo: displayError(job.travelThereTo),
            workFrom: displayError(job.workFrom),
            workTo: displayError(job.workTo),
            travelBackFrom: displayError(job.travelBackFrom),
            travelBackTo: displayError(job.travelBackTo),
          })),
          warranty: displayError(state.formState.warranty),
          noFaults: displayError(state.formState.noFaults),
        }));
    }
  };
  const [state, dispatch] = useReducer(reducer, { type: "initial" });

  const [signatureTheirs, setSignatureTheirs] = useState(false);
  const [signatureOurs, setSignatureOurs] = useState(false);
  const signatureTheirsRef = useRef(
    /** @type { null | SignatureCanvas } */ (null)
  );
  const signatureOursRef = useRef(
    /** @type { null | SignatureCanvas } */ (null)
  );

  const [initialLoaded, setInitialLoaded] = useState(
    /** @type {boolean} */ (false)
  );

  const downloadData = async () => {
    const urlParams = new URLSearchParams(window.location.search);
    const id = urlParams.get("id");
    const idInt = Number(id);
    if (id && !Number.isNaN(idInt)) {
      try {
        /** @type { import("axios").AxiosResponse<import("./Data.t").Payload<import("./Data.t").Upkeep>> } */
        const response = await axios.get(`/tsapi/data/${id}`);
        const data = response.data;
        /** @type { import("./Data.t").Upkeep } */
        const upkeep = {
          ...data,
          date: new Date(data.date),
        };
        const defaultDate = `${upkeep.date.getDate()}. ${
          upkeep.date.getMonth() + 1
        }. ${upkeep.date.getFullYear()}`;

        const newJob = newItem("jobs");

        /** @type { import("./Data.t").ParsedValue<boolean | null, boolean> } */
        const boolean = {
          displayError: false,
          result: {
            type: "error",
            error: "musí být vyplněno",
          },
          value: null,
        };

        /** @type { import("./Data.t").FormState } */
        const formState = {
          employees: upkeep.employees,
          km: 0,
          transport: "reality",
          mileages: {},
          jobs: [
            {
              ...newJob,
              date: {
                ...newJob.date,
                value: defaultDate,
                result: {
                  type: "ok",
                  value: DateTime.fromJSDate(upkeep.date),
                },
              },
            },
          ],
          parts: [newItem("parts")],
          description: "",
          recommendation: "",
          noFaults: boolean,
          warranty: boolean,
        };
        dispatch({
          type: "set_success",
          upkeep,
          upkeepId: idInt,
          formState: formState,
        });
      } catch {
        dispatch({ type: "set_failure" });
      }
    } else {
      dispatch({ type: "set_failure" });
    }
  };

  /** @type { import("react").ChangeEventHandler<HTMLInputElement> } */
  const handleTransport = (e) => {
    dispatch({
      type: "set_transport",
      transport: /** @type { import("./Data.t").Transport} */ (e.target.value),
    });
  };

  useEffect(() => {
    downloadData();
    setTimeout(() => {
      setInitialLoaded(true);
    }, 500);
  }, []);

  /** @type {(data: import("./Data.t").Upkeep, state: import("./Data.t").FormState, upkeepId: number) => React.ReactElement} */
  const renderData = (data, state, upkeepId) => {
    const submitSignature = async () => {
      const ours = signatureTheirsRef.current
        ?.getSignaturePad()
        .toDataURL("image/jpeg");
      const theirs = signatureTheirsRef.current
        ?.getSignaturePad()
        .toDataURL("image/jpeg");
      if (ours !== undefined && theirs !== undefined) {
        /** @type { import("./Data.t").Signatures } */
        const body = { ours, theirs };
        await axios.put(`/tsapi/upload/1`, body);
      }
    };

    const submitData = async () => {
      const anyJobError = state.jobs.some((job) =>
        validatedJobKeys.some((k) => job[k].result.type === "error")
      );
      if (
        anyJobError ||
        state.noFaults.result.type === "error" ||
        state.warranty.result.type === "error"
      ) {
        dispatch({ type: "display_errors" });
      } else {
        /** @type {(job: import("./Data.t").ValidatedJob) => import("./Data.t").SubmitJob[]} */
        const traverseJob = (job) => {
          const r = validatedJobKeys.reduce(
            (acc, key) => {
              const result = job[key].result;
              if (result.type === "ok" && acc.length === 1) {
                return [{ ...acc[0], [key]: result.value }];
              } else {
                return [];
              }
            },
            /** @type {SubmitJob[]} */ [{ date: job.date }]
          );
          // @ts-ignore
          return {...r[0], note: job.note};
        };

        /** @type {import("./Data.t").ParsedForm} */
        const parsedForm = {
          ...state,
          employees: state.employees.flatMap((e) => (e !== null ? [e] : [])),
          warranty: state.warranty.result.value,
          noFaults: state.noFaults.result.value,
          jobs: state.jobs.flatMap((job) => traverseJob(job)),
        };

        await axios.put(`/tsapi/data/${upkeepId}`, parsedForm);
        // window.location.href = window.location.href + `&signature=true`;
      }
    };

    const header = div({ className: "row" }, [
      div({ className: "col-lg-8 mb-4" }, [
        strong("Zákazník"),
        " ",
        span(
          { className: "position-relative", style: { top: "1rem" } },
          data.company_name
        ),
      ]),
      div({ className: "col-lg-4" }, [
        strong("Ujeté km"),
        div({ className: "form-check" }, [
          label(
            { className: "form-check-label", htmlFor: "transport_reality" },
            "Dle skutečnosti"
          ),
          input({
            className: "form-check-input",
            type: "radio",
            name: "transport",
            id: "transport_reality",
            value: "reality",
            onChange: handleTransport,
            checked: state.transport === "reality",
          }),
        ]),
        div({ className: "form-check" }, [
          input({
            className: "form-check-input",
            type: "radio",
            name: "transport",
            id: "transport_km",
            value: "km",
            onChange: handleTransport,
            checked: state.transport === "km",
          }),
          label({ className: "form-check-label", htmlFor: "transport_km" }, [
            input({
              className: "form-control",
              type: "number",
              value: state.km,
              onChange: (e) =>
                dispatch({ type: "set_km", km: Number(e.target.value) }),
            }),
            "Km",
          ]),
        ]),
      ]),
    ]);

    /** @type {(rows: number, field: Extract<keyof import("./Data.t").FormState, "description" | "recommendation">, label: string) => React.ReactElement[]} */
    const mkTextarea = (rows, field, label) => [
      div({ className: "col-lg-12" }, strong(label)),
      div(
        { className: "col-lg-12" },
        textarea({
          value: state[field],
          onChange: (e) =>
            dispatch({
              type: "set_string",
              value: e.target.value,
              field,
            }),
          className: "form-control",
          rows,
        })
      ),
    ];

    const textareas = div({ className: "row" }, [
      mkTextarea(8, "description", "Popis činnosti"),
      mkTextarea(3, "recommendation", "Popis činnosti"),
    ]);

    /** @type {(field: Extract<keyof import("./Data.t").FormState, "warranty" | "noFaults">, display: string) => React.ReactElement} */
    const mkCheckboxes = (field, display) => {
      /** @type {(boolean: boolean, value: string, label_ : string) => React.ReactElement} */
      const mkCheckbox = (boolean, value, label_) =>
        div({ className: "form-check form-check-inline" }, [
          input({
            className: "form-check-input",
            type: "radio",
            name: field,
            id: `${field}_${value}`,
            value,
            onChange: () =>
              dispatch({
                type: "set_boolean",
                field,
                value: {
                  displayError: false,
                  value: boolean,
                  result: {
                    type: "ok",
                    value: true,
                  },
                },
              }),
            checked: state[field].value === boolean,
          }),
          label(
            { className: "form-check-label", htmlFor: `${field}_${value}` },
            label_
          ),
        ]);

      const fieldValue = state[field];

      return div({ className: "col-lg-6" }, [
        div([
          strong(display),
          mkCheckbox(true, "yes", "ano"),
          mkCheckbox(false, "no", "Ne"),
        ]),
        fieldValue.displayError && fieldValue.result.type === "error" ? (
          <span>{fieldValue.result.error}</span>
        ) : (
          ""
        ),
      ]);
    };

    const radiosRow = div({ className: "row" }, [
      div({ className: "col-lg-12" }, "Rozhodnutí o závadě"),
      mkCheckboxes("warranty", "Záruční oprava"),
      mkCheckboxes("noFaults", "Zařízení bylo zprovozněno a pracuje bez závad"),
    ]);

    const submitButton = when(
      !signature,
      div(
        { className: "row" },
        div(
          { className: "col-lg-6" },
          button(
            { type: "submit", className: "btn btn-primary btn-lg" },
            "Dokončit"
          )
        )
      )
    );

    const mkSignatures = () => {
      return !signature
        ? []
        : [
            div({ className: "row" }, [
              div({ className: "col-lg-6" }, "Provedenou práci převzal"),
              div({ className: "col-lg-6" }, "Za 2e předal"),
            ]),
            div({ className: "row" }, [
              div(
                { className: "col-lg-6" },
                appProps.data.type === "client"
                  ? h(SignatureCanvas, {
                      canvasProps: {
                        style: { border: "1px solid black" },
                        width: 400,
                        height: 100,
                        className: "sigCanvas",
                      },
                      onEnd: () => setSignatureTheirs(true),
                      ref: (ref) => (signatureTheirsRef.current = ref),
                    })
                  : img({ src: appProps.data.signatures.theirs })
              ),
              div(
                { className: "col-lg-6" },
                appProps.data.type === "client"
                  ? h(SignatureCanvas, {
                      canvasProps: {
                        style: { border: "1px solid black" },
                        width: 400,
                        height: 100,
                        className: "sigCanvas",
                      },
                      onEnd: () => setSignatureOurs(true),
                      ref: (ref) => (signatureOursRef.current = ref),
                    })
                  : img({ src: appProps.data.signatures.ours })
              ),
            ]),
            div(
              { className: "row mt-5 mb-5" },
              div(
                { className: "col-lg" },
                button(
                  {
                    disabled: !signatureOurs || !signatureTheirs,
                    type: "submit",
                    className: "btn btn-primary btn-lg",
                  },
                  "Nahrát"
                )
              )
            ),
          ];
    };

    return form(
      {
        onSubmit: async (event) => {
          event.preventDefault();
          if (signature) {
            await submitSignature();
          } else {
            await submitData();
          }
        },
      },
      div({ className: "container-lg" }, [
        header,
        renderEmployees(
          signature,
          state.employees,
          data.available_employees,
          dispatch
        ),
        ...renderMachines(signature, data.machines, state.mileages, dispatch),
        ...renderJobs(signature, state.jobs, dispatch),
        ...renderParts(signature, state.parts, data.machines, dispatch),
        textareas,
        radiosRow,
        submitButton,
        ...mkSignatures(),
      ])
    );
  };

  const renderFailure = () => {
    return div("Failed");
  };

  const renderInitial = () => {
    return div(
      { className: "container-lg" },
      div(
        {
          className: "row",
        },
        div({ className: "col" }, [
          div(
            { className: "d-flex justify-content-center" },
            div(
              {
                className: "spinner-border mt-5",
                style: { width: "5rem", height: "5rem" },
              },
              span(
                {
                  className: "visually-hidden",
                },
                "Načítá se..."
              )
            )
          ),
          p({ className: "text-center mt-5" }, "Načítá se..."),
        ])
      )
    );
  };

  if (!initialLoaded) {
    return renderInitial();
  } else {
    switch (state.type) {
      case "success":
        return renderData(state.upkeep, state.formState, state.upkeepId);
      case "failure":
        return renderFailure();
      case "initial":
        return renderInitial();
    }
  }
};

export const AppWithSignature = () => {
  const urlParams = new URLSearchParams(window.location.search);
  const signature = urlParams.get("signature");
  return h(App, { data: { type: "client" }, signature: signature === "true" });
};
