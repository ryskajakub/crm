import "./App.css";
import "bootstrap/dist/css/bootstrap.css";

import { useEffect, useReducer, useState } from "react";
import axios from "axios";
import { DateTime } from "luxon";

/**
 * @typedef { { type: "initial" } } Initial
 * @typedef { { type: "success", upkeep: import("./Data.t").Upkeep, formState: import("./Data.t").FormState } } Success
 * @typedef { { type: "failure" } } Failure
 * @typedef { Initial | Success | Failure } State
 *
 * @typedef { { type: "set_employees", employees: (number | null)[] } } SetEmployees
 * @typedef { { type: "set_km", km: number } } SetKm
 * @typedef { { type: "set_transport", transport: import("./Data.t").Transport } } SetTransport
 * @typedef { { type: "set_failure" } } SetFailure
 * @typedef { { type: "set_success", upkeep: import("./Data.t").Upkeep, formState: import("./Data.t").FormState } } SetSuccess
 * @typedef { { type: "set_mileage", machine_id: number, mileage: number } } SetMileage
 * @typedef { { type: "set_job", index: number, field: keyof import("./Data.t").Job, value: string, result: import("./Data.t").ParsedValue<DateTime>["result"] } } SetJob
 * @typedef { { type: "set_job_note", index: number, value: string } } SetJobNote
 * @typedef { { type: "set_part", index: number, field: Exclude<keyof import("./Data.t").Part, "machine_id">, value: string } } SetPart
 * @typedef { { type: "set_part_link", index: number, machine_id: number | null } } SetPartLink
 * @typedef { { type: "set_string", field: Extract<keyof import("./Data.t").FormState, "recommendation" | "description">, value: string } } SetString
 * @typedef { { type: "set_boolean", field: Extract<keyof import("./Data.t").FormState, "warranty" | "noFaults">, value: boolean } } SetBoolean
 * @typedef { { type: "remove_at_index", field: Extract<keyof import("./Data.t").FormState, "jobs" | "parts" | "employees">, index: number } } RemoveAtIndex
 * @typedef { { type: "display_errors" } } DisplayErrors
 * @typedef { { type: "add_item", field: Extract<keyof import("./Data.t").FormState, "jobs" | "parts" | "employees"> } } AddItem
 * @typedef { SetEmployees | SetKm | SetTransport | SetFailure | SetSuccess | SetMileage | SetJob | SetPart | SetPartLink | SetString | SetBoolean | RemoveAtIndex | AddItem | DisplayErrors | SetJobNote } Action
 *
 */

/**
 * @template {Extract<keyof import("./Data.t").FormState, "jobs" | "parts" | "employees">} T
 * @param {T} field
 * @returns { import("./Data.t").Unpack<import("./Data.t").FormState[T]> }
 */
function newItem(field) {
  /** @type {import("./Data.t").ParsedValue<any>} */
  const newValidatedField = {
    value: "",
    displayError: false,
    result: {
      type: "error",
      error: "",
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
 * @param { import("./Data.t").Part[] } parts
 * @param { Pick<import("./Data.t").Unpack<import("./Data.t").Upkeep["machines"]>, "machine_id" | "serial_number" | "manufacturer">[] } machines
 * @param { React.Dispatch<Action> } dispatch
 * @returns { React.ReactElement }
 */
function renderParts(parts, machines, dispatch) {
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
    return (
      <div key={index} className="row">
        <div className="col-lg-1">{index + 1}</div>
        <div className="col-lg">
          <input
            type="text"
            className="form-control"
            value={parts[index].number}
            onChange={onChange("number")}
          />
        </div>
        <div className="col-lg">
          <input
            type="text"
            className="form-control"
            value={parts[index].name}
            onChange={onChange("name")}
          />
        </div>
        <div className="col-lg-1">
          <input
            type="text"
            className="form-control"
            value={parts[index].quantity}
            onChange={onChange("quantity")}
          />
        </div>
        <div className="col-lg-2">
          <select
            value={part.machine_id || "---"}
            onChange={(e) => {
              const value = e.target.value;
              const valueParsed = value === "---" ? null : Number(value);
              /** @type { SetPartLink } */
              const setParkLink = {
                type: "set_part_link",
                index,
                machine_id: valueParsed,
              };
              dispatch(setParkLink);
            }}
            className="form-select"
          >
            <option key={null} value="---">
              ---
            </option>
            {machines.map((m) => {
              return (
                <option key={m.machine_id} value={m.machine_id}>
                  {m.manufacturer} {m.serial_number}
                </option>
              );
            })}
          </select>
        </div>
        <div className="col-lg-1">
          <button
            className="btn btn-danger"
            type="button"
            onClick={() =>
              dispatch({ type: "remove_at_index", field: "parts", index })
            }
          >
            <svg
              xmlns="http://www.w3.org/2000/svg"
              width="16"
              height="16"
              fill="currentColor"
              className="bi bi-dash-lg"
              viewBox="0 0 16 16"
            >
              <path
                fillRule="evenodd"
                d="M2 8a.5.5 0 0 1 .5-.5h11a.5.5 0 0 1 0 1h-11A.5.5 0 0 1 2 8Z"
              />
            </svg>
          </button>
        </div>
      </div>
    );
  });
  return (
    <>
      {partsRendered}
      <div className="row">
        <div className="col-lg-12">
          <button
            type="button"
            className="btn btn-primary"
            onClick={() => {
              dispatch({ type: "add_item", field: "parts" });
            }}
          >
            <svg
              xmlns="http://www.w3.org/2000/svg"
              width="16"
              height="16"
              fill="currentColor"
              className="bi bi-plus"
              viewBox="0 0 16 16"
            >
              <path d="M8 4a.5.5 0 0 1 .5.5v3h3a.5.5 0 0 1 0 1h-3v3a.5.5 0 0 1-1 0v-3h-3a.5.5 0 0 1 0-1h3v-3A.5.5 0 0 1 8 4z" />
            </svg>
          </button>
        </div>
      </div>
    </>
  );
}

/**
 * @param { import("./Data.t").ValidatedJob[] } jobs
 * @param { React.Dispatch<Action> } dispatch
 * @returns { React.ReactElement }
 */
function renderJobs(jobs, dispatch) {
  const jobsRendered = jobs.map((job, index) => {
    /** @type { (field: keyof import("./Data.t").Job, parse: (value: string) => import("./Data.t").ParsedValue<DateTime>["result"]) => import("react").ChangeEventHandler<HTMLInputElement> } */
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

    /** @type { (inputValue: string) => import("./Data.t").ParsedValue<DateTime>["result"] } */
    const parseDate = (inputValue) => {
      const value = DateTime.fromFormat(inputValue, "d.L.y");

      /** @type { import("./Data.t").ParsedValue<DateTime>["result"] } */
      return value.isValid
        ? { type: "ok", value }
        : { type: "error", error: "špatné datum" };
    };

    /** @type { (inputValue: string) => import("./Data.t").ParsedValue<DateTime>["result"] } */
    const parseTime = (inputValue) => {
      const value = DateTime.fromFormat(inputValue, "H:m");

      if (value.isValid) {
        return { type: "ok", value };
      } else {
        const value2 = DateTime.fromFormat(inputValue, "H");

        /** @type { import("./Data.t").ParsedValue<DateTime>["result"] } */
        return value2.isValid
          ? { type: "ok", value: value2 }
          : { type: "error", error: "špatný čas" };
      }
    };

    /** { Extract<keyof Job, "travelThereFrom" | "travelThereTo" | "workFrom" | "workTo" | "travelBackFrom" | "travelBackTo">[] } */
    const timeKeys = /** @type {const} */ ([
      "travelThereFrom",
      "travelThereTo",
      "workFrom",
      "workTo",
      "travelBackFrom",
      "travelBackTo",
    ]);

    return (
      <div key={index} className="row">
        <div className="col-lg-2">
          <input
            type="text"
            className="form-control"
            onChange={onChange("date", parseDate)}
            value={job.date.value}
            id={`date${index}`}
          />
          {job.date.displayError && job.date.result.type === "error" && (
            <label htmlFor={`date${index}`} className="text-danger">
              {job.date.result.error}
            </label>
          )}
        </div>

        {timeKeys.map((timeKey) => {
          const result = job[timeKey].result;
          return (
            <div key={timeKey} className="col-lg-1">
              <input
                type="text"
                className="form-control"
                onChange={onChange(timeKey, parseTime)}
                value={job[timeKey].value}
                id={`${timeKey}${index}`}
              />
              {job[timeKey].displayError && result.type === "error" && (
                <label htmlFor={`${timeKey}${index}`} className="text-danger">
                  {result.error}
                </label>
              )}
            </div>
          );
        })}
        <div className="col-lg">
          <input
            type="text"
            className="form-control"
            onChange={(e) => {
              /** @type { SetJobNote } */
              const setJobNote = {
                type: "set_job_note",
                index,
                value: e.target.value,
              };
              dispatch(setJobNote);
            }}
            value={job.note}
          />
        </div>
        <div className="col-lg-1">
          {index !== 0 && (
            <button
              className="btn btn-danger"
              type="button"
              onClick={() =>
                dispatch({ type: "remove_at_index", field: "jobs", index })
              }
            >
              <svg
                xmlns="http://www.w3.org/2000/svg"
                width="16"
                height="16"
                fill="currentColor"
                className="bi bi-dash-lg"
                viewBox="0 0 16 16"
              >
                <path
                  fillRule="evenodd"
                  d="M2 8a.5.5 0 0 1 .5-.5h11a.5.5 0 0 1 0 1h-11A.5.5 0 0 1 2 8Z"
                />
              </svg>
            </button>
          )}
        </div>
      </div>
    );
  });
  return (
    <>
      {jobsRendered}
      <div className="row">
        <div className="col-lg-12">
          <button
            className="btn btn-danger"
            type="button"
            onClick={() => {
              dispatch({ type: "add_item", field: "jobs" });
            }}
          >
            <svg
              xmlns="http://www.w3.org/2000/svg"
              width="16"
              height="16"
              fill="currentColor"
              className="bi bi-calendar-plus"
              viewBox="0 0 16 16"
            >
              <path d="M8 7a.5.5 0 0 1 .5.5V9H10a.5.5 0 0 1 0 1H8.5v1.5a.5.5 0 0 1-1 0V10H6a.5.5 0 0 1 0-1h1.5V7.5A.5.5 0 0 1 8 7z" />
              <path d="M3.5 0a.5.5 0 0 1 .5.5V1h8V.5a.5.5 0 0 1 1 0V1h1a2 2 0 0 1 2 2v11a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2V3a2 2 0 0 1 2-2h1V.5a.5.5 0 0 1 .5-.5zM1 4v10a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1V4H1z" />
            </svg>
          </button>
        </div>
      </div>
    </>
  );
}

/**
 * @param { import("./Data.t").Upkeep["machines"] } machines
 * @param { React.Dispatch<Action> } dispatch
 * @returns { React.ReactElement }
 */
function renderMachines(machines, dispatch) {
  const rows = machines.map((machine) => {
    return (
      <div key={machine.machine_id} className="row">
        <div className="col-lg-4">
          Typ zařízení: {machine.manufacturer} {machine.type}
        </div>
        <div className="col-lg-4">v. č. {machine.serial_number}</div>
        <div className="col-lg-2">
          <input
            type="number"
            className="form-control"
            value={machine.mileage}
            onChange={(e) => {
              const value = Number(e.target.value);
              dispatch({
                type: "set_mileage",
                machine_id: machine.machine_id,
                mileage: value,
              });
            }}
          />
        </div>
      </div>
    );
  });
  return <>{rows}</>;
}

/**
 * @param { (number | null)[] } employees
 * @param { import("./Data.t").Upkeep["available_employees"] } available_employees
 * @param { React.Dispatch<Action> } dispatch
 * @returns { React.ReactElement }
 */
function renderEmployees(employees, available_employees, dispatch) {
  /** @type { (newEmployeesState: (number | null)[]) => void } */
  const setEmployees = (newEmployeesState) => {
    dispatch({ type: "set_employees", employees: newEmployeesState });
  };

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
    return (
      <div className="col-lg-2" key={index}>
        <div className="input-group">
          <select
            value={employee || "---"}
            onChange={change}
            className="form-select"
          >
            <option key={null} value="---">
              ---
            </option>
            {available_employees.map((a) => {
              return (
                <option key={a.id} value={a.id}>
                  {a.name}
                </option>
              );
            })}
          </select>
          <button
            className="btn btn-danger"
            type="button"
            onClick={() => {
              dispatch({ type: "remove_at_index", field: "employees", index });
            }}
          >
            <svg
              xmlns="http://www.w3.org/2000/svg"
              width="16"
              height="16"
              fill="currentColor"
              className="bi bi-dash-lg"
              viewBox="0 0 16 16"
            >
              <path
                fillRule="evenodd"
                d="M2 8a.5.5 0 0 1 .5-.5h11a.5.5 0 0 1 0 1h-11A.5.5 0 0 1 2 8Z"
              />
            </svg>
          </button>
        </div>
      </div>
    );
  });

  const newEmployee = (
    <div className="col-lg-2">
      <button
        type="button"
        className="btn btn-primary"
        onClick={() => dispatch({ type: "add_item", field: "employees" })}
      >
        <svg
          xmlns="http://www.w3.org/2000/svg"
          width="16"
          height="16"
          fill="currentColor"
          className="bi bi-person-plus"
          viewBox="0 0 16 16"
        >
          <path d="M6 8a3 3 0 1 0 0-6 3 3 0 0 0 0 6zm2-3a2 2 0 1 1-4 0 2 2 0 0 1 4 0zm4 8c0 1-1 1-1 1H1s-1 0-1-1 1-4 6-4 6 3 6 4zm-1-.004c-.001-.246-.154-.986-.832-1.664C9.516 10.68 8.289 10 6 10c-2.29 0-3.516.68-4.168 1.332-.678.678-.83 1.418-.832 1.664h10z" />
          <path
            fillRule="evenodd"
            d="M13.5 5a.5.5 0 0 1 .5.5V7h1.5a.5.5 0 0 1 0 1H14v1.5a.5.5 0 0 1-1 0V8h-1.5a.5.5 0 0 1 0-1H13V5.5a.5.5 0 0 1 .5-.5z"
          />
        </svg>
      </button>
    </div>
  );

  return (
    <>
      {renderedEmployees}
      {newEmployee}
    </>
  );
}

/**
 * @type { React.FC }
 */
const App = () => {
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
        /** @type { <A>(validatedValue: import("./Data.t").ParsedValue<A> ) => import("./Data.t").ParsedValue<A> } */
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
        }));
    }
  };
  const [state, dispatch] = useReducer(reducer, { type: "initial" });

  const [initialLoaded, setInitialLoaded] = useState(
    /** @type {boolean} */ (false)
  );

  const downloadData = async () => {
    const urlParams = new URLSearchParams(window.location.search);
    const myParam = urlParams.get("id");
    if (myParam) {
      try {
        /** @type { import("axios").AxiosResponse<import("./Data.t").Upkeep> } */
        const response = await axios.get(`/tsapi?id=${myParam}`);
        const data = response.data;
        const upkeep = {
          ...data,
          date: new Date(data.date),
        };
        const defaultDate = `${upkeep.date.getDate()}. ${
          upkeep.date.getMonth() + 1
        }. ${upkeep.date.getFullYear()}`;

        const newJob = newItem("jobs");

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
              },
            },
          ],
          parts: [newItem("parts")],
          description: "",
          recommendation: "",
          noFaults: null,
          warranty: null,
        };
        dispatch({
          type: "set_success",
          upkeep,
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

  /** @type {(data: import("./Data.t").Upkeep, state: import("./Data.t").FormState) => React.ReactElement} */
  const renderData = (data, state) => {
    return (
      <form
        onSubmit={(event) => {
          event.preventDefault();
          dispatch({ type: "display_errors" });
        }}
      >
        <div className="container-lg">
          <div className="row">
            <div className="col-lg-8 mb-4">
              <strong>Zákazník</strong>:{" "}
              <span className="position-relative" style={{ top: "1rem" }}>
                {data.company_name}
              </span>
            </div>
            <div className="col-lg-4">
              <strong>Ujeté km</strong>:
              <div className="form-check">
                <input
                  className="form-check-input"
                  type="radio"
                  name="transport"
                  id="transport_reality"
                  value="reality"
                  onChange={handleTransport}
                  checked={state.transport === "reality"}
                />
                <label className="form-check-label" htmlFor="transport_reality">
                  Dle skutečnosti
                </label>
              </div>
              <div className="form-check">
                <input
                  className="form-check-input"
                  type="radio"
                  name="transport"
                  id="transport_km"
                  value="km"
                  onChange={handleTransport}
                  checked={state.transport === "km"}
                />
                <label className="form-check-label" htmlFor="transport_km">
                  <input
                    className="form-control"
                    type="number"
                    value={state.km}
                    onChange={(e) =>
                      dispatch({ type: "set_km", km: Number(e.target.value) })
                    }
                  />
                  Km
                </label>
              </div>
            </div>
          </div>
          <div className="row">
            <div className="col-lg-2">
              <strong>Jméno technika:</strong>
            </div>
            {renderEmployees(
              state.employees,
              data.available_employees,
              dispatch
            )}
          </div>
          <div className="row">
            <div className="col-lg-4">
              <strong>Typ zařízení</strong>
            </div>
            <div className="col-lg-4">
              <strong>Výrobní číslo</strong>
            </div>
            <div className="col-lg-3">
              <strong>Počet mth</strong>
            </div>
          </div>
          {renderMachines(data.machines, dispatch)}
          <div className="row">
            <div className="col-lg-1">
              <strong>Den, rok</strong>
            </div>
            <div className="col-lg">
              <strong>Cesta tam</strong>
            </div>
            <div className="col-lg">
              <strong>Práce</strong>
            </div>
            <div className="col-lg">
              <strong>Cesta zpět</strong>
            </div>
            <div className="col-lg-2">
              <strong>Poznámky</strong>
            </div>
          </div>
          <div className="row">
            <div className="col-lg-2">Datum</div>
            <div className="col-lg-1">od</div>
            <div className="col-lg-1">do</div>
            <div className="col-lg-1">od</div>
            <div className="col-lg-1">do</div>
            <div className="col-lg-1">od</div>
            <div className="col-lg-1">do</div>
            <div className="col-lg"></div>
            <div className="col-lg-1"></div>
          </div>
          {renderJobs(state.jobs, dispatch)}
          <div className="row">
            <div className="col-lg-1">
              <strong>Pořadí</strong>
            </div>
            <div className="col-lg">
              <strong>Číslo materiálu</strong>
            </div>
            <div className="col-lg">
              <strong>Název materiálu</strong>
            </div>
            <div className="col-lg-1">
              <strong>Množství</strong>
            </div>
            <div className="col-lg-2">
              <strong>Zařízení</strong>
            </div>
            <div className="col-lg-1"></div>
          </div>
          {renderParts(state.parts, data.machines, dispatch)}
          <div className="row">
            <div className="col-lg-12">
              <strong>Popis činnosti</strong>
            </div>
            <div className="col-lg-12">
              <textarea
                value={state.description}
                onChange={(e) =>
                  dispatch({
                    type: "set_string",
                    value: e.target.value,
                    field: "description",
                  })
                }
                className="form-control"
                rows={8}
              ></textarea>
            </div>
            <div className="col-lg-12">
              <strong>Doporučení</strong>
            </div>
            <div className="col-lg-12">
              <textarea
                value={state.recommendation}
                onChange={(e) =>
                  dispatch({
                    type: "set_string",
                    value: e.target.value,
                    field: "recommendation",
                  })
                }
                className="form-control"
                rows={3}
              ></textarea>
            </div>
          </div>
          <div className="row">
            <div className="col-lg-12">Rozhodnutí o závadě</div>
            <div className="col-lg-6">
              <strong>Záruční oprava</strong>
              <div className="form-check form-check-inline">
                <input
                  className="form-check-input"
                  type="radio"
                  name="warranty"
                  id="warranty_yes"
                  value="yes"
                  onChange={() =>
                    dispatch({
                      type: "set_boolean",
                      field: "warranty",
                      value: true,
                    })
                  }
                  checked={state.warranty === true}
                />
                <label className="form-check-label" htmlFor="warranty_yes">
                  Ano
                </label>
              </div>
              <div className="form-check form-check-inline">
                <input
                  className="form-check-input"
                  type="radio"
                  name="warranty"
                  id="warranty_no"
                  value="no"
                  onChange={() =>
                    dispatch({
                      type: "set_boolean",
                      field: "warranty",
                      value: false,
                    })
                  }
                  checked={state.warranty === false}
                />
                <label className="form-check-label" htmlFor="warranty_no">
                  Ne
                </label>
              </div>
            </div>
            <div className="col-lg-6">
              Zařízení bylo zprovozněno a pracuje bez závad
              <div className="form-check form-check-inline">
                <input
                  className="form-check-input"
                  type="radio"
                  name="noFaults"
                  id="noFaults_yes"
                  value="yes"
                  onChange={() =>
                    dispatch({
                      type: "set_boolean",
                      field: "noFaults",
                      value: true,
                    })
                  }
                  checked={state.noFaults === true}
                />
                <label className="form-check-label" htmlFor="noFaults_yes">
                  Ano
                </label>
              </div>
              <div className="form-check form-check-inline">
                <input
                  className="form-check-input"
                  type="radio"
                  name="noFaults"
                  id="noFaults_no"
                  value="no"
                  onChange={() =>
                    dispatch({
                      type: "set_boolean",
                      field: "noFaults",
                      value: false,
                    })
                  }
                  checked={state.noFaults === false}
                />
                <label className="form-check-label" htmlFor="noFaults_no">
                  Ne
                </label>
              </div>
            </div>
          </div>
          <div className="row">
            <div className="col-lg-6">
              <button type="submit" className="btn btn-primary btn-lg">
                Dokončit
              </button>
            </div>
          </div>
          <div className="row">
            <div className="col-lg-6">Provedenou práci převzal</div>
            <div className="col-lg-6">Za 2e předal</div>
          </div>
        </div>
      </form>
    );
  };

  const renderFailure = () => {
    return <div>Failed</div>;
  };

  const renderInitial = () => {
    return (
      <div className="container-lg">
        <div className="row">
          <div className="col">
            <div className="d-flex justify-content-center">
              <div
                className="spinner-border mt-5"
                style={{ width: "5rem", height: "5rem" }}
              >
                <span className="visually-hidden">Načítá se...</span>
              </div>
            </div>
            <p className="text-center mt-5">Načítá se...</p>
          </div>
        </div>
      </div>
    );
  };

  if (!initialLoaded) {
    return renderInitial();
  } else {
    switch (state.type) {
      case "success":
        return renderData(state.upkeep, state.formState);
      case "failure":
        return renderFailure();
      case "initial":
        return renderInitial();
    }
  }
};

export default App;
