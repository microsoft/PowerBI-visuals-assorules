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

    interface VisualSettingsThresholdsParams {
        show: boolean;
        minRuleLength: string;
        maxRuleLength: string;
        threshSupport: number;
        threshConfidence: number;
        threshLift: number;
    }

    interface VisualSettingsRulesParams {
        show: boolean;
        sortBy: string;
        showFrom: number;
        showTo: number;
    }

    interface VisualSettingsVizParams {
        show: boolean;
        visualisationMethod: string;
        rulesPerPlate: string;
        textSize: number;
        edgeColLHS: string;
        edgeColRHS: string;
        colorBy: string;
    }

    interface VisualSettingsAdditionalParams {
        show: boolean;
        showWarnings: boolean;
    }

    export class Visual implements IVisual {
        private imageDiv: HTMLDivElement;
        private imageElement: HTMLImageElement;

        private settings_thresholds_params: VisualSettingsThresholdsParams;
        private settings_rules_params: VisualSettingsRulesParams;
        private settings_viz_params: VisualSettingsVizParams;
        private settings_additional_params: VisualSettingsAdditionalParams;

        public constructor(options: VisualConstructorOptions) {
            this.imageDiv = document.createElement('div');
            this.imageDiv.className = 'rcv_autoScaleImageContainer';
            options.element.appendChild(this.imageDiv);

            this.imageElement = document.createElement('img');
            this.imageElement.className = 'rcv_autoScaleImage';

            this.imageDiv.appendChild(this.imageElement);

            this.settings_thresholds_params = <VisualSettingsThresholdsParams>{
                show: false,
                minRuleLength: "2",
                maxRuleLength: "8",
                threshSupport: 0.01,
                threshConfidence: 0.6,
                threshLift: 1.1
            };
            this.settings_rules_params = <VisualSettingsRulesParams>{
                show: false,
                sortBy: "lift",
                showFrom: 1,
                showTo: 5
            };
            this.settings_viz_params = <VisualSettingsVizParams>{
                show: false,
                visualisationMethod: "graph",
                rulesPerPlate: "1",
                textSize: 10,
                edgeColLHS: "green",
                edgeColRHS: "orange",
                colorBy: "lift"
            };
            this.settings_additional_params = <VisualSettingsAdditionalParams>{
                show: false,
                showWarnings: false
            };
        }

        public update(options: VisualUpdateOptions) {
            let dataViews: DataView[] = options.dataViews;
            if (!dataViews || dataViews.length === 0)
                return;

            let dataView: DataView = dataViews[0];
            if (!dataView || !dataView.metadata)
                return;

            this.settings_thresholds_params = <VisualSettingsThresholdsParams>{
                show: getValue<boolean>(dataView.metadata.objects, 'settings_thresholds_params', 'show', false),
                maxRuleLength: getValue<string>(dataView.metadata.objects, 'settings_thresholds_params', 'maxRuleLength', "8"),
                minRuleLength: getValue<string>(dataView.metadata.objects, 'settings_thresholds_params', 'minRuleLength', "2"),
                threshSupport: getValue<number>(dataView.metadata.objects, 'settings_thresholds_params', 'threshSupport', 0.01),
                threshConfidence: getValue<number>(dataView.metadata.objects, 'settings_thresholds_params', 'threshConfidence', 0.6),
                threshLift: getValue<number>(dataView.metadata.objects, 'settings_thresholds_params', 'threshLift', 1.1)
            };

            this.settings_rules_params = <VisualSettingsRulesParams>{
                show: getValue<boolean>(dataView.metadata.objects, 'settings_rules_params', 'show', false),
                sortBy: getValue<string>(dataView.metadata.objects, 'settings_rules_params', 'sortBy', "lift"),
                showFrom: getValue<number>(dataView.metadata.objects, 'settings_rules_params', 'showFrom', 1),
                showTo: getValue<number>(dataView.metadata.objects, 'settings_rules_params', 'showTo', 5)
            };

            this.settings_viz_params = <VisualSettingsVizParams>{
                show: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'show', false),
                visualisationMethod: getValue<string>(dataView.metadata.objects, 'settings_viz_params', 'visualisationMethod', "graph"),
                rulesPerPlate: getValue<string>(dataView.metadata.objects, 'settings_viz_params', 'rulesPerPlate', "1"),
                textSize: getValue<number>(dataView.metadata.objects, 'settings_viz_params', 'textSize', 10),
                edgeColLHS: getValue<string>(dataView.metadata.objects, 'settings_viz_params', 'edgeColLHS', "green"),
                edgeColRHS: getValue<string>(dataView.metadata.objects, 'settings_viz_params', 'edgeColRHS', "orange"),
                colorBy: getValue<string>(dataView.metadata.objects, 'settings_viz_params', 'colorBy', "lift")
            };
            this.settings_additional_params = <VisualSettingsAdditionalParams> {
                show: getValue<boolean>(dataView.metadata.objects, 'settings_additional_params', 'show', false),
                showWarnings: getValue<boolean>(dataView.metadata.objects, 'settings_additional_params', 'showWarnings', false)
            };

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
            this.imageDiv.style.height = finalViewport.height + 'px';
            this.imageDiv.style.width = finalViewport.width + 'px';
        }

        public enumerateObjectInstances(options: EnumerateVisualObjectInstancesOptions): VisualObjectInstanceEnumeration {
            let objectName = options.objectName;
            let objectEnumeration = [];

            switch (objectName) {
                case 'settings_thresholds_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings_thresholds_params.show,
                            minRuleLength: inMinMaxString(this.settings_thresholds_params.minRuleLength, 2, 10),
                            maxRuleLength: inMinMaxString(this.settings_thresholds_params.maxRuleLength, Number(this.settings_thresholds_params.minRuleLength), 10),
                            threshSupport: inMinMax(this.settings_thresholds_params.threshSupport, 0, 1),
                            threshConfidence: inMinMax(this.settings_thresholds_params.threshConfidence, 0, 1),
                            threshLift: inMinMax(this.settings_thresholds_params.threshLift, 0, 1000000)
                         },
                        selector: null
                    });
                    break;
                    case 'settings_rules_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings_rules_params.show,
                            sortBy: this.settings_rules_params.sortBy,
                            showFrom: this.settings_rules_params.showFrom,
                            showTo: inMinMax(this.settings_rules_params.showTo, this.settings_rules_params.showFrom, 100)
                     },
                        selector: null
                    });
                    break;
                    case 'settings_viz_params':
                    if (this.settings_viz_params.visualisationMethod === "graph") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                show: this.settings_viz_params.show,
                                visualisationMethod: this.settings_viz_params.visualisationMethod,
                                rulesPerPlate: inVisMethodAndRulesPerPlate(this.settings_viz_params.visualisationMethod, this.settings_viz_params.rulesPerPlate),
                                textSize: this.settings_viz_params.textSize,
                                edgeColLHS: this.settings_viz_params.edgeColLHS,
                                edgeColRHS: this.settings_viz_params.edgeColRHS,
                            },
                            selector: null
                        });
                    }
                    else if (this.settings_viz_params.visualisationMethod === "paracoord") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                show: this.settings_viz_params.show,
                                visualisationMethod: this.settings_viz_params.visualisationMethod,
                                colorBy: inVisMethodAndColorBy(this.settings_viz_params.visualisationMethod, this.settings_viz_params.colorBy)
                            },
                            selector: null
                        });
                    }
                    else if (this.settings_viz_params.visualisationMethod === "table") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                show: this.settings_viz_params.show,
                                visualisationMethod: this.settings_viz_params.visualisationMethod,
                                textSize: this.settings_viz_params.textSize
                            },
                            selector: null
                        });
                    }
                    else if (this.settings_viz_params.visualisationMethod === "scatter") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                show: this.settings_viz_params.show,
                                visualisationMethod: this.settings_viz_params.visualisationMethod,
                                textSize: this.settings_viz_params.textSize
                            },
                            selector: null
                        });
                    }

                    break;
                    case 'settings_additional_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings_additional_params.show,
                            showWarnings: this.settings_additional_params.showWarnings,
                         },
                        selector: null
                    });
                    break;
            };

            return objectEnumeration;
        }
    }
}