import { TimeUnit } from '@/store/userInterface'
import { EnumDictionary } from '@/types'

const SECOND = 1
const MINUTE = SECOND * 60
const HOUR = MINUTE * 60
const DAY = HOUR * 24
const WEEK = DAY * 7
const MONTH = HOUR * 30
const YEAR = DAY * 365

export const timeUnitMultipliers: EnumDictionary<TimeUnit, number> = {
  [TimeUnit.SECOND]: SECOND,
  [TimeUnit.MINUTE]: MINUTE,
  [TimeUnit.HOUR]: HOUR,
  [TimeUnit.DAY]: DAY,
  [TimeUnit.WEEK]: WEEK,
  [TimeUnit.MONTH]: MONTH,
  [TimeUnit.YEAR]: YEAR,
}

export const formatDate = (date: Date) => {
  return date.toLocaleString(undefined, {
    year: 'numeric',
    month: 'short',
    day: '2-digit',
    hour: '2-digit',
    hour12: false,
    minute: '2-digit',
    second: '2-digit',
  })
}

export const shiftDatetime = (
  datetime: number,
  amount: number,
  unit: TimeUnit
) => {
  if (unit === TimeUnit.MONTH) {
    const currentDate = new Date(datetime)
    currentDate.setMonth(currentDate.getMonth() + amount)
    return currentDate.getTime()
  } else if (unit === TimeUnit.YEAR) {
    const currentDate = new Date(datetime)
    currentDate.setFullYear(currentDate.getFullYear() + 1)
    return currentDate.getTime()
  } else {
    return datetime + amount * timeUnitMultipliers[unit] * 1000
  }
}
