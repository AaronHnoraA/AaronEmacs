const {getDefaultConfig, mergeConfig} = require('@react-native/metro-config');
const path = require('path');

const platformsRoot = path.resolve(__dirname, '../../platforms/react-native');
const demoNodeModules = path.resolve(__dirname, 'node_modules');

// Escape path for use in RegExp
const escaped = platformsRoot.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');

const config = {
  watchFolders: [platformsRoot],
  resolver: {
    // Prevent Metro from resolving modules inside platforms/react-native/node_modules.
    // That directory has its own react-native peer dep which would create a second
    // RN instance and break PlatformConstants / TurboModule lookup.
    blockList: new RegExp(`^${escaped}/node_modules/.*`),
    nodeModulesPaths: [demoNodeModules],
  },
};

module.exports = mergeConfig(getDefaultConfig(__dirname), config);
