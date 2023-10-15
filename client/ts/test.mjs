import { DateTime } from "luxon"



const dt = DateTime.fromFormat("10:00:00", "H:mm:ss")

console.log(dt.get('hour'))

const d1 = "2022-08-20T10:09:33.839Z"

console.log(DateTime.fromISO(d1).toJSON())