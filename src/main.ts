import { createApp } from 'vue'
import './style.css'
import App from './App.vue'
import { _Window } from './types'

// @ts-ignore
;(window as _Window).someFunction = (message: string) => {
  alert(message)
}

createApp(App).mount('#app')
