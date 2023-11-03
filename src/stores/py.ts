import { defineStore } from 'pinia'
import { _Window } from '@/types'
import { Ref, ref } from 'vue'

const _window = window as _Window

export const usePyStore = defineStore('py', () => {
  const interpreter: Ref<any> = ref(null)
  const initialized = ref(false)
  const startEl = document.getElementById('start') // Used to detect when PyScript has finished initializing

  const _init = () => {
    setTimeout(() => {
      interpreter.value = _window.pyscript.interpreter
      initialized.value = true

      // Cleanup
      startEl?.removeEventListener('click', _init)
      startEl?.remove()
    }, 0)
  }

  const deleteDataPoint = (
    timeseries: (string | number)[][],
    index: number
  ) => {
    const handleDelete = interpreter.value.globals.get('handleDelete')
    handleDelete?.(timeseries, index)
  }

  const addOne = (timeseries: (string | number)[][], index: number) => {
    const handleAddOne = interpreter.value.globals.get('handleAddOne')
    handleAddOne?.(timeseries, index)
  }

  if (startEl) {
    startEl.onclick = _init
  } else {
    throw 'Failed to detect PyScript initialization'
  }

  return {
    // Getters
    interpreter,
    initialized,

    // Actions
    deleteDataPoint,
    addOne,
  }
})
