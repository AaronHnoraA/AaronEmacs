module.exports = {
  dependency: {
    platforms: {
      ios: {},
      android: {
        sourceDir: './android',
        packageImportPath: 'import io.ratex.RaTeXPackage;',
        packageInstance: 'new RaTeXPackage()',
      },
    },
  },
};
