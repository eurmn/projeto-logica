import { defineConfig, presetIcons, presetWind, transformerDirectives } from 'unocss';

export default defineConfig({
  presets: [
    presetWind(),
    presetIcons(),
  ],
  transformers: [
    transformerDirectives()
  ],
  theme: {
    fontFamily: {
      'sans': 'InterVariable',
      'title': 'Familjen GroteskVariable',
    },
  }
});