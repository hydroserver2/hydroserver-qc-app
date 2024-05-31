<template>
  <v-row class="my-2" align="center">
    <v-col>
      <h5 class="text-h5">Datastreams</h5>
    </v-col>

    <v-col cols="12" sm="3" class="ml-auto">
      <v-select
        label="Show/Hide columns"
        v-model="selectedHeaders"
        :items="selectableHeaders"
        item-text="title"
        item-value="key"
        multiple
        item-color="green"
        density="compact"
        variant="solo"
        hide-details
      >
        <template v-slot:selection="{ item, index }">
          <!-- Leave blank so nothing appears in the v-select box -->
        </template>
      </v-select>
    </v-col>
  </v-row>

  <v-card>
    <v-toolbar flat color="secondary-lighten-2">
      <v-text-field
        class="mx-2"
        clearable
        v-model="search"
        prepend-inner-icon="mdi-magnify"
        label="Search"
        hide-details
        density="compact"
        rounded="xl"
      />

      <v-spacer />

      <v-btn @click="clearSelected"> Clear Selected </v-btn>

      <v-btn
        variant="outlined"
        rounded="xl"
        @click="showOnlySelected = !showOnlySelected"
      >
        {{ showOnlySelected ? 'Show All' : 'Show Selected' }}
      </v-btn>

      <v-btn
        :loading="downloading"
        prepend-icon="mdi-download"
        @click="downloadSelected(selectedDatastreams)"
        >Download Selected</v-btn
      >
    </v-toolbar>
    <v-data-table-virtual
      :headers="headers.filter((header) => header.visible)"
      :items="tableItems"
      :sort-by="sortBy"
      multi-sort
      :search="search"
      :style="{ 'max-height': `${tableHeight}vh` }"
      fixed-header
      class="elevation-2"
      color="green"
      density="compact"
      @click:row="onRowClick"
      hover
    >
      <template v-slot:item.plot="{ item }">
        <v-checkbox
          :model-value="isChecked(item)"
          :disabled="
            (selectedDatastreams.length >= 5 && !isChecked(item)) ||
            isSelected(item)
          "
          class="d-flex align-self-center"
          density="compact"
          @change="() => updatePlottedDatastreams(item)"
        />
      </template>
      <template v-slot:item.select="{ item }">
        <v-checkbox
          :model-value="isSelected(item)"
          :disabled="selectedDatastreams.length >= 5 && !isChecked(item)"
          class="d-flex align-self-center"
          density="compact"
          @change="() => updateSelectedDatastream(item)"
        />
      </template>
    </v-data-table-virtual>
  </v-card>

  <v-dialog v-model="openInfoCard" width="50rem" v-if="selectedDatastream">
    <DatastreamInformationCard
      :datastream="selectedDatastream"
      @close="openInfoCard = false"
    />
  </v-dialog>
</template>

<script setup lang="ts">
import { useDataVisStore } from '@/store/dataVisualization'
import { Datastream } from '@/types'
import { storeToRefs } from 'pinia'
import { computed, reactive, ref } from 'vue'
import DatastreamInformationCard from './DatastreamInformationCard.vue'
import { downloadSelectedDatastreamsCSVs } from '@/utils/CSVDownloadUtils'
import { useUIStore } from '@/store/userInterface'

const { tableHeight } = storeToRefs(useUIStore())
const {
  things,
  filteredDatastreams,
  selectedDatastreams,
  observedProperties,
  processingLevels,
  qcDatastream,
} = storeToRefs(useDataVisStore())

const showOnlySelected = ref(false)
const openInfoCard = ref(false)
const downloading = ref(false)
const selectedDatastream = ref<Datastream | null>(null)

const downloadSelected = async (selectedDatastreams: Datastream[]) => {
  downloading.value = true
  try {
    await downloadSelectedDatastreamsCSVs(selectedDatastreams)
  } catch (error) {
    console.error('Error downloading selected datastreams', error)
  }
  downloading.value = false
}

const onRowClick = (event: Event, item: any) => {
  // If the click came from a checkbox, do nothing.
  let targetElement = event.target as HTMLElement
  if (targetElement.id && targetElement.id.startsWith('checkbox-')) return

  const selectedDatastreamId = item.item.id
  const foundDatastream = filteredDatastreams.value.find(
    (d) => d.id === selectedDatastreamId
  )
  if (foundDatastream) {
    selectedDatastream.value = foundDatastream
    openInfoCard.value = true
  } else selectedDatastream.value = null
}

const displayDatastreams = computed(() => {
  if (showOnlySelected.value) {
    return filteredDatastreams.value.filter((ds) =>
      selectedDatastreams.value.some((sds) => sds.id === ds.id)
    )
  } else {
    return filteredDatastreams.value
  }
})

const tableItems = computed(() => {
  return displayDatastreams.value.map((ds) => {
    const thing = things.value.find((t) => t.id === ds.thingId)
    const observedProperty = observedProperties.value.find(
      (p) => p.id === ds.observedPropertyId
    )
    const processingLevel = processingLevels.value.find(
      (p) => p.id === ds.processingLevelId
    )
    return {
      ...ds,
      siteCodeName: thing?.samplingFeatureCode,
      observedPropertyName: observedProperty?.name,
      qualityControlLevelDefinition: processingLevel?.definition,
    }
  })
})

function clearSelected() {
  showOnlySelected.value = false
  selectedDatastreams.value = []
}

const isChecked = (item: Datastream) =>
  selectedDatastreams.value.some((sds) => sds.id === item.id)

const isSelected = (ds: Datastream) => ds.id === qcDatastream.value?.id

const search = ref()
const headers = reactive([
  { title: 'Plot', key: 'plot', visible: true },
  { title: 'Select', key: 'select', visible: true },
  {
    title: 'Site code',
    key: 'siteCodeName',
    visible: true,
  },
  {
    title: 'Observed property',
    key: 'observedPropertyName',
    visible: true,
  },
  {
    title: 'Processing level',
    key: 'qualityControlLevelDefinition',
    visible: true,
  },
  {
    title: 'Number observations',
    key: 'valueCount',
    visible: true,
  },
  {
    title: 'Date last updated',
    key: 'phenomenonEndTime',
    visible: true,
  },
])

const selectableHeaders = computed(() => {
  return headers.filter(
    (header) => header.key !== 'plot' && header.key !== 'select'
  )
})

const sortBy = [
  { key: 'siteCodeName' },
  { key: 'observedPropertyName' },
  { key: 'qualityControlLevelDefinition' },
]
const selectedHeaders = computed({
  get: () =>
    headers.filter((header) => header.visible).map((header) => header.key),
  set: (keys) => {
    headers.forEach((header) => {
      header.visible = keys.includes(header.key)
    })
  },
})

const findIndexInPlotted = (ds: Datastream) =>
  selectedDatastreams.value.findIndex((item) => item.id === ds.id)

const addDatastreamToPlotted = (ds: Datastream) => {
  const index = findIndexInPlotted(ds)
  if (index === -1) selectedDatastreams.value.push(ds)
}

function updatePlottedDatastreams(datastream: Datastream) {
  const index = findIndexInPlotted(datastream)
  if (index === -1) selectedDatastreams.value.push(datastream)
  else selectedDatastreams.value.splice(index, 1)
}

function updateSelectedDatastream(datastream: Datastream) {
  // Case 1: No currently selected & selecting
  if (qcDatastream.value === null) {
    addDatastreamToPlotted(datastream)
    qcDatastream.value = datastream
    return
  }

  // Case 2: There is a currently selected
  // Case 2.1: There's an unsaved history - open a modal that says this action will delete their unsaved work.
  // TODO - Or do we want a watcher in the dataVis store that does this check reactively?
  // Probably two watchers so we're looking at selectedDatastreams.
  // If the qc datastream gets deselected, then check or when someone changed
  // the qcDatastream's value check and revert if there's a history

  // if (!historyResolved()) {
  //   return
  // }

  // Case 2.2: There's no history and we're unselecting it
  if (datastream.id === qcDatastream.value.id) {
    qcDatastream.value = null
    return
  }

  // Case 2.3: Switching datastreams
  addDatastreamToPlotted(datastream)
  qcDatastream.value = datastream
}
</script>
