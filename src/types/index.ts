import { useDataVisStore } from '@/store/dataVisualization'
import { useEChartsStore } from '@/store/echarts'
import { Operator, TimeUnit, usePyStore } from '@/store/py'
import { fetchObservationsParallel } from '@/utils/observationsUtils'
import { LineSeriesOption } from 'echarts'
import { storeToRefs } from 'pinia'

export type EnumDictionary<T extends string | symbol | number, U> = {
  [K in T]: U
}

export type DataPoint = {
  date: Date
  value: number
  qualifierValue: string[]
}

export interface PartialQualifier {
  code: string
  description: string
}

export type Qualifier = {
  qualityCode: string | null
  resultQualifiers: PartialQualifier[]
}

export type Observation = [string, number, Qualifier]

export type DataArray = Observation[]

export enum EnumEditOperations {
  ADD_POINTS = 'ADD_POINTS',
  CHANGE_VALUES = 'CHANGE_VALUES',
  DELETE_POINTS = 'DELETE_POINTS',
  DRIFT_CORRECTION = 'DRIFT_CORRECTION',
  INTERPOLATE = 'INTERPOLATE',
  SHIFT_DATETIMES = 'SHIFT_DATETIMES',
  FIND_GAPS = 'FIND_GAPS',
  FILL_GAPS = 'FILL_GAPS',
}

export enum EnumFilterOperations {
  FIND_GAPS = 'FIND_GAPS',
}

export class ObservationRecord {
  // A JsProxy of the pandas DataFrame
  dataFrame: any
  /** The generated dataset to be used in echarts */
  dataset: { dimensions: string[]; source: { [key: string]: any[] } } = {
    dimensions: [],
    source: {},
  }
  history: { method: EnumEditOperations; args?: any[]; icon: string }[]
  isLoading: boolean
  ds: Datastream

  constructor(dataArray: any[], ds: Datastream) {
    const { instantiateDataFrame } = usePyStore()
    const components = ['date', 'value', 'qualifier']
    this.history = []
    this.ds = ds

    this.dataFrame = instantiateDataFrame(dataArray, components)

    this.isLoading = false
  }

  /**
   * Reloads the DataFrame
   */
  async reload() {
    const { instantiateDataFrame } = usePyStore()
    const { beginDate, endDate } = storeToRefs(useDataVisStore())

    const fetchedData = await fetchObservationsParallel(
      this.ds,
      beginDate.value,
      endDate.value
    )

    const components = ['date', 'value', 'qualifier']

    this.dataFrame = instantiateDataFrame(fetchedData, components)
    this.history = []
  }

  /**
   * @param index
   * @returns
   */
  async reloadHistory(index: number) {
    const newHistory = this.history.slice(0, index + 1)
    await this.reload()

    await this.dispatch(newHistory.map((h) => [h.method, ...(h.args || [])]))
    return
  }

  /**
   * Remove a history item
   * @param index
   */
  async removeHistoryItem(index: number) {
    const newHistory = [...this.history]
    newHistory.splice(index, 1)
    await this.reload()
    await this.dispatch(newHistory.map((h) => [h.method, ...(h.args || [])]))
    return
  }

  /** This is an expensive operation and should be only executed when necessary */
  generateDataset() {
    console.log('generateDataset')
    const components = [
      'date',
      'value',
      //  'qualifier'
    ]
    this.dataset = {
      dimensions: components,
      source: {
        date: (Array.from(this.dataFrame.get_date_column()) as number[]) || [],
        value:
          (Array.from(this.dataFrame.get_value_column()) as number[]) || [],
        // qualifier: Array.from(
        //   this.dataFrame.get_qualifier_column()
        // ) as string[][],
      },
    }
  }

  get beginTime() {
    const beginDateTime = this.dataFrame.get_datetime_at(0)
    return new Date(beginDateTime)
  }

  get endTime() {
    const endDateTime = this.dataFrame.get_datetime_at(
      this.dataFrame.count() - 1
    )
    return new Date(endDateTime)
  }

  /** Dispatch an operation and log its signature in hisotry */
  async dispatch(
    action: EnumEditOperations | [EnumEditOperations, ...any][],
    ...args: any
  ) {
    const { editHistory } = storeToRefs(useEChartsStore())
    const actions: EnumDictionary<EnumEditOperations, Function> = {
      [EnumEditOperations.ADD_POINTS]: this._addDataPoints,
      [EnumEditOperations.CHANGE_VALUES]: this._changeValues,
      [EnumEditOperations.DELETE_POINTS]: this._deleteDataPoints,
      [EnumEditOperations.DRIFT_CORRECTION]: this._driftCorrection,
      [EnumEditOperations.INTERPOLATE]: this._interpolate,
      [EnumEditOperations.SHIFT_DATETIMES]: this._shift,
      [EnumEditOperations.FIND_GAPS]: this._findGaps,
      [EnumEditOperations.FILL_GAPS]: this._fillGaps,
    }

    // TODO: consolidate with icons in EditDrawer component
    const editIcons: EnumDictionary<EnumEditOperations, string> = {
      [EnumEditOperations.ADD_POINTS]: 'mdi-plus',
      [EnumEditOperations.CHANGE_VALUES]: 'mdi-pencil',
      [EnumEditOperations.DELETE_POINTS]: 'mdi-trash-can',
      [EnumEditOperations.DRIFT_CORRECTION]: 'mdi-chart-sankey',
      [EnumEditOperations.INTERPOLATE]: 'mdi-transit-connection-horizontal',
      [EnumEditOperations.SHIFT_DATETIMES]: 'mdi-calendar',
      [EnumEditOperations.FILL_GAPS]: 'mdi-keyboard-space',
      [EnumEditOperations.FIND_GAPS]: 'mdi-keyboard-space',
    }

    let response = []

    try {
      if (Array.isArray(action)) {
        for (let i = 0; i < action.length; i++) {
          const method = action[i][0]
          const args = action[i].slice(1, action[i].length)
          const res = await actions[method].apply(this, args)
          response.push(res)
          this.history.push({ method, args, icon: editIcons[method] })
        }
        editHistory.value = [...this.history]
      } else {
        response = await actions[action].apply(this, args)
        this.history.push({ method: action, args, icon: editIcons[action] })
        editHistory.value = [...this.history]
      }
    } catch (e) {
      console.log(
        `Failed to execute operation: ${action} with arguments: `,
        args
      )
      console.log(e)
    }

    this.generateDataset()
    return response
  }

  /** Filter operations do not transform the data */
  async dispatchFilter(
    action: EnumFilterOperations | [EnumFilterOperations, ...any][],
    ...args: any
  ) {
    const filters: EnumDictionary<EnumFilterOperations, Function> = {
      [EnumFilterOperations.FIND_GAPS]: this._findGaps,
    }
    let response = []

    try {
      if (Array.isArray(action)) {
        for (let i = 0; i < action.length; i++) {
          const method = action[i][0]
          const args = action[i].slice(1, action[i].length)
          const res = await filters[method].apply(this, args)
          response.push(res)
        }
      } else {
        response = await filters[action].apply(this, args)
      }
    } catch (e) {
      console.log(
        `Failed to execute filter operation: ${action} with arguments: `,
        args
      )
      console.log(e)
    }
    return response
  }

  /**
   * @param index An array containing the list of index of values to perform the operations on.
   * @param operator The operator that will be applied
   * @param value The value to use in the operation
   * @returns The modified DataFrame
   */
  _changeValues(index: number[], operator: Operator, value: number) {
    this.dataFrame.change_values(index, operator, value)
  }

  _interpolate(index: number[]) {
    this.dataFrame.interpolate(index)
  }

  /**
   * Shifts the selected indexes by a constant
   * @param index The index list of entries to shift
   * @param amount Number of {@link TimeUnit}
   * @param unit {@link TimeUnit}
   * @returns
   */
  _shift(index: number[], amount: number, unit: TimeUnit) {
    this.dataFrame.shift_points(index, amount, unit)
  }

  /**
   * Find gaps in the data
   * @param value The time value
   * @param unit The time unit (TimeUnit)
   * @returns
   */

  _findGaps(value: number, unit: TimeUnit, range?: [number, number]) {
    return this.dataFrame.find_gaps(value, unit, range)
  }

  /**
   * Find gaps and fill them with placeholder value
   * @param gap Intervals to detect as gaps
   * @param fill Interval used to fill the detected gaps
   * @param interpolateValues If true, the new values will be linearly interpolated
   * @returns
   */
  _fillGaps(
    gap: [number, TimeUnit],
    fill: [number, TimeUnit],
    interpolateValues: boolean,
    range?: [number, number]
  ) {
    return this.dataFrame.fill_gaps(gap, fill, interpolateValues, range)
  }

  /**
   *
   * @param index The index list of entries to shift
   */
  _deleteDataPoints(index: number[]) {
    this.dataFrame.delete_data_points(index)
  }

  /**
   *
   * @param start The start index
   * @param end The end index
   * @param value The drift amount
   */
  _driftCorrection(start: number, end: number, value: number) {
    this.dataFrame.drift_correction(start, end, value)
  }

  /**
   * @param dataPoints
   */
  _addDataPoints(
    dataPoints: [
      string,
      number,
      Partial<{
        resultQualifiers: string[]
      }>
    ][]
  ) {
    this.dataFrame.add_points(dataPoints)
  }
}

export interface GraphSeries {
  id: string
  name: string
  // data: DataPoint[]
  data: ObservationRecord // dataframe
  yAxisLabel: string
  seriesOption: LineSeriesOption
}

export type TimeSpacingUnit = 'seconds' | 'minutes' | 'hours' | 'days'

export interface Owner {
  firstName: string
  lastName: string
  organizationName: string
  isPrimaryOwner: boolean
  email: string
}

export interface Tag {
  id: string
  key: string
  value: string
}

export type Frequency = 'daily' | 'weekly' | 'monthly' | null

export class HydroShareArchive {
  id: string
  thingId: string
  link: string
  frequency: Frequency
  path: string
  datastreamIds: string[]
  publicResource: boolean

  constructor() {
    this.id = ''
    this.thingId = ''
    this.link = ''
    this.frequency = null
    this.path = 'HydroShare'
    this.datastreamIds = []
    this.publicResource = false
  }
}

export class PostHydroShareArchive extends HydroShareArchive {
  resourceTitle?: string
  resourceAbstract?: string
  resourceKeywords?: string[]

  constructor() {
    super()
    this.resourceTitle = undefined
    this.resourceAbstract = undefined
    this.resourceKeywords = undefined
  }
}

export class Thing {
  id: string
  name: string
  owners: Owner[]
  tags: Tag[]
  hydroShareArchive?: HydroShareArchive | null
  siteType: string
  samplingFeatureCode: string
  isPrivate: boolean
  latitude?: number | ''
  longitude?: number | ''
  elevation_m?: number | ''
  elevationDatum: string
  ownsThing: boolean
  followsThing: boolean
  description: string
  samplingFeatureType: string
  state: string
  county: string
  country: string
  isPrimaryOwner: boolean
  dataDisclaimer: string

  constructor() {
    this.id = ''
    this.name = ''
    this.owners = []
    this.tags = []
    this.siteType = ''
    this.samplingFeatureCode = ''
    this.isPrivate = false
    this.elevationDatum = 'WGS84'
    this.ownsThing = false
    this.followsThing = false
    this.description = ''
    this.samplingFeatureType = 'Site'
    this.state = ''
    this.county = ''
    this.country = ''
    this.isPrimaryOwner = false
    this.dataDisclaimer = ''
  }
}

export interface ThingWithColor extends Thing {
  color?: {
    borderColor: string
    background: string
    glyphColor: string
  }
  tagValue?: string
}

export class Datastream {
  id: string
  name: string
  description: string
  thingId: string
  observationType: string
  resultType?: string
  status?: string
  sampledMedium: string
  noDataValue: number
  aggregationStatistic: string
  unitId: string
  observedPropertyId: string
  sensorId: string
  processingLevelId: string
  isVisible: boolean
  isDataVisible: boolean
  phenomenonBeginTime?: string | null
  phenomenonEndTime?: string | null
  intendedTimeSpacing?: number
  intendedTimeSpacingUnits?: string | null
  timeAggregationInterval: number | null
  timeAggregationIntervalUnitsId: string
  dataSourceId?: string | null
  dataSourceColumn?: string | number | null
  valueCount: number

  constructor(thingId?: string) {
    this.id = ''
    this.name = ''
    this.description = ''
    this.thingId = thingId || ''
    this.observationType = 'OM_Measurement'
    this.resultType = 'Time Series Coverage'
    this.sampledMedium = ''
    this.noDataValue = -9999
    this.aggregationStatistic = ''
    this.unitId = ''
    this.observedPropertyId = ''
    this.sensorId = ''
    this.processingLevelId = ''
    this.timeAggregationInterval = null
    this.timeAggregationIntervalUnitsId = ''
    this.isVisible = true
    this.valueCount = 0
    this.isDataVisible = true
  }
}

export class Unit {
  id: string
  owner: string | null
  name: string
  symbol: string
  definition: string
  type: string

  constructor() {
    this.id = ''
    this.owner = null
    this.name = ''
    this.symbol = ''
    this.definition = ''
    this.type = ''
  }
}

export class Sensor {
  id: string
  owner: string | null
  name: string
  description: string
  manufacturer: string
  model: string
  methodType: string
  methodCode: string
  methodLink: string
  encodingType: string
  modelLink: string

  constructor() {
    this.id = ''
    this.owner = null
    this.name = ''
    this.description = ''
    this.manufacturer = ''
    this.model = ''
    this.methodType = 'Instrument Deployment'
    this.methodCode = ''
    this.methodLink = ''
    this.encodingType = 'application/json'
    this.modelLink = ''
  }
}

export class ObservedProperty {
  id: string
  name: string
  owner: string | null
  definition: string
  description: string
  type: string
  code: string

  constructor() {
    this.id = ''
    this.name = ''
    this.owner = null
    this.definition = ''
    this.description = ''
    this.type = 'Hydrology'
    this.code = ''
  }
}

export class ProcessingLevel {
  id: string
  owner: string | null
  code: string
  definition: string
  explanation: string

  constructor() {
    this.id = ''
    this.owner = null
    this.code = ''
    this.definition = ''
    this.explanation = ''
  }
}

export class ResultQualifier {
  id: string
  owner: string | null
  code: string
  description: string

  constructor() {
    this.id = ''
    this.owner = null
    this.code = ''
    this.description = ''
  }
}

export class DataSource {
  id: string
  name: string
  path: string
  url: string | null
  headerRow?: number
  dataStartRow: number
  delimiter: string
  interval: number | null
  intervalUnits: string | null
  crontab: string
  startTime: string | null
  endTime: string | null
  paused: boolean
  timestampColumn: string | number
  timestampFormat: string
  timestampOffset: string
  dataLoaderId: string
  dataSourceThru: string | null
  lastSyncSuccessful: boolean
  lastSyncMessage: string
  lastSynced: string | null
  nextSync: string | null

  constructor() {
    this.id = ''
    this.name = ''
    this.path = ''
    this.url = null
    this.dataStartRow = 1
    this.delimiter = ','
    this.interval = null
    this.intervalUnits = null
    this.crontab = ''
    this.startTime = null
    this.endTime = null
    this.paused = false
    this.timestampColumn = ''
    this.timestampFormat = ''
    this.timestampOffset = ''
    this.dataLoaderId = ''
    this.dataSourceThru = null
    this.lastSyncSuccessful = false
    this.lastSyncMessage = ''
    this.lastSynced = null
    this.nextSync = null
  }
}

export class DataLoader {
  id: string
  name: string

  constructor() {
    this.id = ''
    this.name = ''
  }
}

export class Organization {
  name?: string
  code?: string
  type?: string
  description?: string
  link?: string

  constructor() {}
}

export class User {
  id: string
  email: string
  password: string
  firstName: string
  middleName: string
  lastName: string
  phone: string
  address: string
  organization?: Organization | null
  type: string
  isVerified: boolean
  link: string
  hydroShareConnected: boolean

  constructor() {
    this.id = ''
    this.email = ''
    this.password = ''
    this.firstName = ''
    this.middleName = ''
    this.lastName = ''
    this.phone = ''
    this.address = ''
    this.type = ''
    this.isVerified = false
    this.link = ''
    this.hydroShareConnected = false
  }
}

export interface DatastreamMetadata {
  units: Unit[]
  sensors: Sensor[]
  processingLevels: ProcessingLevel[]
  observedProperties: ObservedProperty[]
}

export interface Photo {
  id: string
  thingId: string
  filePath: string
  link: string
}

export enum OAuthProvider {
  google = 'google',
  orcid = 'orcid',
  hydroshare = 'hydroshare',
}

export type _Window = Window &
  typeof globalThis & { pyscript: any; loadPyodide: any; pyGlobals: any } & {
    [key: string]: any
  }
