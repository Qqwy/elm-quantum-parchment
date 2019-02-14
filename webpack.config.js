const webpack = require('webpack');
const path = require('path');
const webpackMerge = require('webpack-merge');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin')
// const StyleLintPlugin = require('stylelint-webpack-plugin');

const modeConfig = env => require(`./build-utils/webpack.${env}`)(env);
const presetConfig = require("./build-utils/loadPresets");

module.exports = ({ mode, presets, asset_path} = { mode: "production", presets: [], asset_path: "/"}) => {
  console.log(`Building for: ${mode}`);

// Try the environment variable, otherwise use root
console.log('asset_path', asset_path); // 'local'

  return webpackMerge(
    {
      mode,

      entry: {
        main: path.join(__dirname, './src/index.js'),
        vendor: path.join(__dirname, './src/assets/js/vendor.js'),
      },
        devServer: {
            disableHostCheck: true,
        },
        module: {
            rules: [
                {
                    test: /\.(woff(2)?|ttf|eot|svg)(\?v=\d+\.\d+\.\d+)?$/,
                    use: [{
                        loader: 'file-loader',
                        options: {
                            name: '[name].[ext]',
                            outputPath: 'fonts/'
                        }
                    }]
                }
            ]
        },
        output: {
            publicPath: asset_path
        },

      plugins: [
        new HtmlWebpackPlugin({
          template: 'src/assets/index.html',
          inject: 'body',
          filename: 'index.html',
        }),

        // new StyleLintPlugin(),

        new CopyWebpackPlugin([
          { from: 'src/assets/favicon.ico' }
        ]),
      ]
    },
    modeConfig(mode),
    presetConfig({ mode, presets }),
  )
};
