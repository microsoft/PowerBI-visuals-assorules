/*
 *  Power BI Visual CLI
 *
 *  Copyright (c) Microsoft Corporation
 *  All rights reserved.
 *  MIT License
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the ""Software""), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED *AS IS*, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 *  THE SOFTWARE.
 */
module powerbi.extensibility.visual {
    "use strict";
    export class Visual implements IVisual {
        private imageDiv: HTMLDivElement;
        private imageElement: HTMLImageElement;
        private settings: VisualSettings;

        public constructor(options: VisualConstructorOptions) {
            this.imageDiv = document.createElement("div");
            this.imageDiv.className = "rcv_autoScaleImageContainer";
            this.imageElement = document.createElement("img");
            this.imageElement.className = "rcv_autoScaleImage";
            this.imageDiv.appendChild(this.imageElement);
            options.element.appendChild(this.imageDiv);
        }

        public update(options: VisualUpdateOptions): void {
            if (!options ||
                !options.type ||
                !options.viewport ||
                !options.dataViews ||
                options.dataViews.length === 0 ||
                !options.dataViews[0]) {
                return;
            }
            const dataView: DataView = options.dataViews[0];

            this.settings = Visual.parseSettings(dataView);

            let imageUrl: string = null;
            if (dataView.scriptResult && dataView.scriptResult.payloadBase64) {
                imageUrl = "data:image/png;base64," + dataView.scriptResult.payloadBase64;
            }

            if (imageUrl) {
                this.imageElement.src = imageUrl;
            } else {
                this.imageElement.src = null;
            }

            this.onResizing(options.viewport);
        }

        public onResizing(finalViewport: IViewport): void {
            this.imageDiv.style.height = finalViewport.height + "px";
            this.imageDiv.style.width = finalViewport.width + "px";
        }

        private static parseSettings(dataView: DataView): VisualSettings {
            return VisualSettings.parse(dataView) as VisualSettings;
        }
        /** 
         * This function gets called for each of the objects defined in the capabilities files and allows you to select which of the 
         * objects and properties you want to expose to the users in the property pane.
         */
        // public enumerateObjectInstances(options: EnumerateVisualObjectInstancesOptions):
        //     VisualObjectInstance[] | VisualObjectInstanceEnumerationObject {
        //     return VisualSettings.enumerateObjectInstances(this.settings || VisualSettings.getDefault(), options);
        // }
        public enumerateObjectInstances(options: EnumerateVisualObjectInstancesOptions): VisualObjectInstanceEnumeration {
            let objectName = options.objectName;
            let objectEnumeration = [];

            switch (objectName) {
                case 'settings_thresholds_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                          //  show: this.settings_thresholds_params.show,
                            minRuleLength: inMinMaxString(this.settings.settings_thresholds_params.minRuleLength, 2, 10),
                            maxRuleLength: inMinMaxString(this.settings.settings_thresholds_params.maxRuleLength, Number(this.settings.settings_thresholds_params.minRuleLength), 10),
                            threshSupport: inMinMax(this.settings.settings_thresholds_params.threshSupport, 0, 1),
                            threshConfidence: inMinMax(this.settings.settings_thresholds_params.threshConfidence, 0, 1),
                            threshLift: inMinMax(this.settings.settings_thresholds_params.threshLift, 0, 1000000)
                         },
                        selector: null
                    });
                    break;
                    case 'settings_rules_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                          //  show: this.settings.settings_rules_params.show,
                            sortBy: this.settings.settings_rules_params.sortBy,
                            showFrom: this.settings.settings_rules_params.showFrom,
                            showTo: inMinMax(this.settings.settings_rules_params.showTo, this.settings.settings_rules_params.showFrom, 100)
                     },
                        selector: null
                    });
                    break;
                    case 'settings_viz_params':
                    if (this.settings.settings_viz_params.visualisationMethod === "graph") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                               // show: this.settings.settings_viz_params.show,
                                visualisationMethod: this.settings.settings_viz_params.visualisationMethod,
                                rulesPerPlate: inVisMethodAndRulesPerPlate(this.settings.settings_viz_params.visualisationMethod, this.settings.settings_viz_params.rulesPerPlate),
                                textSize: this.settings.settings_viz_params.textSize,
                                edgeColLHS: this.settings.settings_viz_params.edgeColLHS,
                                edgeColRHS: this.settings.settings_viz_params.edgeColRHS,
                            },
                            selector: null
                        });
                    }
                    else if (this.settings.settings_viz_params.visualisationMethod === "paracoord") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                              //  show: this.settings.settings_viz_params.show,
                                visualisationMethod: this.settings.settings_viz_params.visualisationMethod,
                                colorBy: inVisMethodAndColorBy(this.settings.settings_viz_params.visualisationMethod, this.settings.settings_viz_params.colorBy)
                            },
                            selector: null
                        });
                    }
                    else if (this.settings.settings_viz_params.visualisationMethod === "table") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                               // show: this.settings.settings_viz_params.show,
                                visualisationMethod: this.settings.settings_viz_params.visualisationMethod,
                                textSize: this.settings.settings_viz_params.textSize
                            },
                            selector: null
                        });
                    }
                    else if (this.settings.settings_viz_params.visualisationMethod === "scatter") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                              //  show: this.settings.settings_viz_params.show,
                                visualisationMethod: this.settings.settings_viz_params.visualisationMethod,
                                textSize: this.settings.settings_viz_params.textSize
                            },
                            selector: null
                        });
                    }

                    break;
                    case 'settings_additional_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                           // show: this.settings.settings_additional_params.show,
                            showWarnings: this.settings.settings_additional_params.showWarnings,
                         },
                        selector: null
                    });
                    break;
            };

            return objectEnumeration;
        }
    }
}