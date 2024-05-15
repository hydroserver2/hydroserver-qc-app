<template>
  <v-row align="center" class="no-wrap">
    <v-col cols="auto">
      <SquareBtn
        class="mr-1"
        v-for="option in dateOptions"
        :color="
          selectedDateBtnId === option.id ? 'blue' : 'blue-grey-lighten-4'
        "
        :icon="option.icon"
        :label="option.label"
        btn-width="4rem"
        variant="outlined"
        @click="onDateBtnClick(option.id)"
      />
    </v-col>
    <v-col cols="auto" class="datepicker">
      <DatePickerField
        :model-value="beginDate"
        placeholder="Begin Date"
        @update:model-value="setDateRange({ begin: $event })"
      />
    </v-col>
    <v-col cols="auto" class="datepicker">
      <DatePickerField
        :model-value="endDate"
        placeholder="End Date"
        @update:model-value="setDateRange({ end: $event })"
      />
    </v-col>
  </v-row>
</template>

<script setup lang="ts">
import DatePickerField from '@/components/VisualizeData/DatePickerField.vue'
import SquareBtn from '@/components/SquareBtn.vue'
import { useDataVisStore } from '@/store/dataVisualization'
import { storeToRefs } from 'pinia'

const { setDateRange, onDateBtnClick } = useDataVisStore()
const { dateOptions, beginDate, endDate, selectedDateBtnId } = storeToRefs(
  useDataVisStore()
)
</script>

<style scoped>
.datepicker {
  min-width: 175px;
}
</style>
