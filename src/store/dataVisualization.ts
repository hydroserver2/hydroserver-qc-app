import { Datastream, ObservedProperty, ProcessingLevel, Thing } from '@/types'
import { defineStore, storeToRefs } from 'pinia'
import { computed, ref, watch } from 'vue'
import { useEChartsStore } from './echarts'

export const useDataVisStore = defineStore('dataVisualization', () => {
  const {
    resetChartZoom,
    updateVisualization,
    clearChartState,
    fetchGraphSeries,
  } = useEChartsStore()

  const { graphSeriesArray, prevIds } = storeToRefs(useEChartsStore())

  // To only fetch these once per page
  const things = ref<Thing[]>([])
  const datastreams = ref<Datastream[]>([])
  const observedProperties = ref<ObservedProperty[]>([])
  const processingLevels = ref<ProcessingLevel[]>([])

  // Filters
  const selectedThings = ref<Thing[]>([])
  const selectedObservedPropertyNames = ref<string[]>([])
  const selectedProcessingLevelNames = ref<string[]>([])

  // Datasets
  /** The datastream selected to go through the quality control process */
  const qcDatastream = ref<Datastream | null>(null)
  const plottedDatastreams = ref<Datastream[]>([])

  // Qualifiers
  const qualifierSet = ref<Set<string>>(new Set())
  const selectedQualifier = ref('')

  /** Track the loading status of each datastream to be plotted.
   * Set to true when we get a response from the API. Keyed by datastream id. */
  const loadingStates = ref(new Map<string, boolean>())

  // Time range
  const endDate = ref<Date>(new Date())
  const oneWeek = 7 * 24 * 60 * 60 * 1000
  const beginDate = ref<Date>(new Date(endDate.value.getTime() - oneWeek))
  const selectedDateBtnId = ref(2)

  function resetState() {
    selectedThings.value = []
    plottedDatastreams.value = []
    selectedObservedPropertyNames.value = []
    selectedProcessingLevelNames.value = []
    endDate.value = new Date()
    beginDate.value = new Date(new Date().getTime() - oneWeek)
    selectedDateBtnId.value = 2
    resetChartZoom()
  }

  function matchesSelectedObservedProperty(datastream: Datastream) {
    if (selectedObservedPropertyNames.value.length === 0) return true

    const OPName = observedProperties.value.find(
      (op) => op.id === datastream.observedPropertyId
    )?.name
    return (
      OPName !== undefined &&
      selectedObservedPropertyNames.value.includes(OPName)
    )
  }

  function matchesSelectedProcessingLevel(datastream: Datastream) {
    if (selectedProcessingLevelNames.value.length === 0) return true

    const PLName = processingLevels.value.find(
      (pl) => pl.id === datastream.processingLevelId
    )?.definition
    return (
      PLName !== undefined &&
      selectedProcessingLevelNames.value.includes(PLName)
    )
  }

  function matchesSelectedThing(datastream: Datastream) {
    if (selectedThings.value.length === 0) return true

    return (
      selectedThings.value.length === 0 ||
      selectedThings.value.some((thing) => thing.id === datastream.thingId)
    )
  }

  const filteredDatastreams = computed(() => {
    return datastreams.value.filter(
      (datastream) =>
        matchesSelectedThing(datastream) &&
        matchesSelectedObservedProperty(datastream) &&
        matchesSelectedProcessingLevel(datastream)
    )
  })

  const dateOptions = ref([
    {
      id: 0,
      icon: 'mdi-calendar',
      label: 'Last Year',
      calculateBeginDate: () => {
        const now = endDate.value
        return new Date(now.getFullYear() - 1, now.getMonth(), now.getDate())
      },
    },
    {
      id: 1,
      icon: 'mdi-calendar-month',
      label: 'Last Month',
      calculateBeginDate: () => {
        const now = endDate.value
        return new Date(now.getFullYear(), now.getMonth() - 1, now.getDate())
      },
    },
    {
      id: 2,
      icon: 'mdi-calendar-week',
      label: 'Last Week',
      calculateBeginDate: () => {
        const now = endDate.value
        return new Date(now.getFullYear(), now.getMonth(), now.getDate() - 7)
      },
    },
  ])

  const getMostRecentEndTime = () =>
    plottedDatastreams.value.reduce((latest, ds) => {
      const dsEndDate = new Date(ds.phenomenonEndTime!)
      return dsEndDate > latest ? dsEndDate : latest
    }, new Date(0))

  interface SetDateRangeParams {
    begin?: Date
    end?: Date
    update?: boolean
    custom?: boolean
  }

  const setDateRange = ({
    begin,
    end,
    update = true,
    custom = true,
  }: SetDateRangeParams) => {
    resetChartZoom()
    if (begin) beginDate.value = begin
    if (end) endDate.value = end

    if (custom) selectedDateBtnId.value = -1

    if (update) {
      clearChartState()
      if (!beginDate || !endDate || !plottedDatastreams.value.length) return
      updateDatasets(plottedDatastreams.value)
    }
  }

  const onDateBtnClick = (selectedId: number) => {
    const selectedOption = dateOptions.value.find(
      (option) => option.id === selectedId
    )
    if (selectedOption) {
      const newEndDate = getMostRecentEndTime()
      const newBeginDate = selectedOption.calculateBeginDate()

      selectedDateBtnId.value = selectedId
      setDateRange({
        begin: newBeginDate,
        end: newEndDate,
        custom: false,
      })
    }
  }

  const fetchDatasets = (datastreams: Datastream[]) => {
    datastreams.forEach((ds) => {
      loadingStates.value.set(ds.id, true)
      const begin = beginDate.value.toISOString()
      const end = endDate.value.toISOString()
      fetchGraphSeries(ds, begin, end)
        .then((newSeries) => {
          if (!plottedDatastreams.value.some((sd) => sd.id === ds.id)) return

          graphSeriesArray.value = graphSeriesArray.value.filter(
            (series) => series.id !== ds.id
          )

          graphSeriesArray.value.push(newSeries)
          updateVisualization()
        })
        .catch((error) => {
          console.error(`Failed to fetch dataset ${ds.id}:`, error)
        })
        .finally(() => {
          loadingStates.value.set(ds.id, false)
        })
    })
  }

  const updateDatasets = async (datastreams: Datastream[]) => {
    const currentIds = datastreams.map((ds) => ds.id)
    const newIds = currentIds.filter((id) => !prevIds.value.includes(id))
    const removedIds = prevIds.value.filter((id) => !currentIds.includes(id))

    // Remove old
    if (removedIds.length) {
      graphSeriesArray.value = graphSeriesArray.value.filter(
        (series) => !removedIds.includes(series.id)
      )
      updateVisualization()
    }

    // fetch new
    if (newIds.length)
      fetchDatasets(datastreams.filter((d) => newIds.includes(d.id)))
  }

  // If currently selected datastreams are no longer in filteredDatastreams, deselect them
  watch(
    () => filteredDatastreams.value,
    (newDatastreams) => {
      plottedDatastreams.value = plottedDatastreams.value.filter((ds) =>
        newDatastreams.some((datastream) => datastream.id === ds.id)
      )
    },
    { deep: true }
  )

  // Set the time range for the qcDatastream if there is one, otherwise
  // update the time range to the most recent phenomenon end time
  let prevDatastreamIds = ''
  watch(
    () => plottedDatastreams.value,
    (newDs) => {
      const newDatastreamIds = JSON.stringify(newDs.map((ds) => ds.id).sort())

      if (!newDs.length || !beginDate.value || !endDate.value) {
        clearChartState()
      } else if (newDatastreamIds !== prevDatastreamIds) {
        const oldEnd = endDate.value
        const oldBegin = beginDate.value

        endDate.value = qcDatastream.value
          ? new Date(qcDatastream.value.phenomenonEndTime!)
          : getMostRecentEndTime()

        const selectedOption = dateOptions.value.find(
          (option) => option.id === selectedDateBtnId.value
        )

        // Set beginDate based on previous time range
        if (selectedOption) {
          beginDate.value = selectedOption.calculateBeginDate()
        } else {
          const timeDifference = oldEnd.getTime() - oldBegin.getTime()
          beginDate.value = new Date(endDate.value.getTime() - timeDifference)
        }

        if (
          oldEnd.getTime() !== endDate.value.getTime() ||
          oldBegin.getTime() !== beginDate.value.getTime()
        )
          clearChartState()
        updateDatasets(newDs)
      }
      prevDatastreamIds = newDatastreamIds
    },
    { deep: true, immediate: true }
  )

  // TODO: Revisit this. Does it make sense to convert qualifierValue to a string in preprocessing
  // just to split it into an array of strings here? Maybe just save it as an array of strings instead
  function updateQualifiers() {
    const series = graphSeriesArray.value.find(
      (s) => s.id === qcDatastream.value?.id
    )

    qualifierSet.value = new Set([])
    if (series) {
      for (const dataPoint of series.data) {
        if (typeof dataPoint.qualifierValue === 'string') {
          // Split the qualifierValue string into individual qualifiers and add them to the set
          dataPoint.qualifierValue
            .split(',')
            .forEach((qualifier) => qualifierSet.value.add(qualifier.trim()))
        }
      }
    }
    selectedQualifier.value = ''
  }

  // Update qualifiers whenever the qcDatastream's graphSeries has finished loading
  let previousLoadingState = false
  watch(
    [loadingStates],
    () => {
      const currentId = qcDatastream.value?.id
      if (!currentId) return
      const currentLoadingState = !!loadingStates.value.get(currentId)
      if (!currentLoadingState && previousLoadingState) updateQualifiers()
      previousLoadingState = currentLoadingState
    },
    { deep: true }
  )

  return {
    things,
    datastreams,
    processingLevels,
    observedProperties,
    selectedThings,
    selectedObservedPropertyNames,
    selectedProcessingLevelNames,
    filteredDatastreams,
    plottedDatastreams,
    beginDate,
    endDate,
    dateOptions,
    loadingStates,
    selectedDateBtnId,
    qcDatastream,
    qualifierSet,
    selectedQualifier,
    matchesSelectedObservedProperty,
    matchesSelectedProcessingLevel,
    matchesSelectedThing,
    setDateRange,
    onDateBtnClick,
    resetState,
  }
})
