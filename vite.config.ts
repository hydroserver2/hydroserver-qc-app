import { defineConfig } from 'vite'
import { resolve } from 'path'
import { VitePWA } from 'vite-plugin-pwa'
import vue from '@vitejs/plugin-vue'
import dsv from '@rollup/plugin-dsv'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [
    vue(),
    // VitePWA({
    //   registerType: 'autoUpdate',
    //   selfDestroying: true,
    //   workbox: {
    //     globPatterns: ['**/*.{js,css,html,png,svg}'],
    //   },
    //   injectRegister: 'inline',
    //   devOptions: {
    //     enabled: true,
    //     /* other options */
    //   },
    //   manifest: {
    //     name: 'HydroServer QC App',
    //     short_name: 'QcApp',
    //     description: 'HydroServer QC App',
    //     theme_color: '#ffffff',
    //     icons: [
    //       {
    //         src: 'vite.svg',
    //         sizes: '192x192',
    //         type: 'image/svg+xml',
    //       },
    //     ],
    //   },
    // }),
    dsv(),
  ],
  server: {
    // These headers are required to enable workers
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer#security_requirements
    headers: {
      'Cross-Origin-Opener-Policy': 'same-origin',
      'Cross-Origin-Resource-Policy': 'cross-origin',
      'Cross-Origin-Embedder-Policy': 'require-corp',
    },
  },
  resolve: {
    extensions: ['.js', '.json', '.vue', '.less', '.scss', '.ts', '.py'],
    alias: {
      '@': resolve(__dirname, 'src'),
    },
  },
})
