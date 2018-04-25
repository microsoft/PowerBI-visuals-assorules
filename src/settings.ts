/*
 *  Power BI Visualizations
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
    import DataViewObjectsParser = powerbi.extensibility.utils.dataview.DataViewObjectsParser;


 /**
     * Gets property value for a particular object.
     *
     * @function
     * @param {DataViewObjects} objects - Map of defined objects.
     * @param {string} objectName       - Name of desired object.
     * @param {string} propertyName     - Name of desired property.
     * @param {T} defaultValue          - Default value of desired property.
     */
    export function getValueMinMax<T>(objects: DataViewObjects, objectName: string, propertyName: string, defaultValue: T, minVal: T, maxVal: T ): T {
      if (objects) {
          let object = objects[objectName];
          if (object) {
              let property: T = <T>object[propertyName];
              if (property < minVal) {
                  return minVal;
              }
              if (property > maxVal) {
                  return maxVal;
              }
              if (property !== undefined) {
                  return property;
              }
          }
      }
      return defaultValue;
  }


   /**
   * Gets property value for a particular object.
   *
   * @function
   * @param {DataViewObjects} objects - Map of defined objects.
   * @param {string} objectName       - Name of desired object.
   * @param {string} propertyName     - Name of desired property.
   * @param {T} defaultValue          - Default value of desired property.
   */
  export function getValueNumberMinMax(objects: DataViewObjects, objectName: string, propertyName: string, defaultValue: number, minValue: number, maxValue: number ) {
      if (objects) {
          let object = objects[objectName];
          if (object) {
              let property = object[propertyName];
              if (property !== undefined) {
                  if (property > maxValue) {
                      return maxValue;
                  }
                  if (property < minValue) {
                      return minValue;
                  }
                  return property;
              }
          }
      }
      return defaultValue;
  }



  export function inMinMax(a: number, mi: number, ma: number) {
      if (a < mi)
          return mi;
      if (a > ma)
          return ma;
      return a;
  }

  export function inMinMaxString(a: string, mi: number, ma: number) {
      if (Number(a) < mi)
          return mi.toString();
      if (Number(a) > ma)
          return ma.toString();
      return a;
  }

  export function inVisMethodAndRulesPerPlate(method: string, rulesPerPlate: string) {
      if (method === "graph" && rulesPerPlate === "N/A")
          return "1";
      if (method !== "graph")
          return "N/A";
      return rulesPerPlate;
  }
  export function inVisMethodAndColorBy(method: string, colorBy: string) {
      if (method === "paracoord" && colorBy === "N/A")
          return "lift";
      if (method !== "paracoord")
          return "N/A";
      return colorBy;
  }

  /**
   * Gets property value for a particular object in a category.
   *
   * @function
   * @param {DataViewCategoryColumn} category - List of category objects.
   * @param {number} index                    - Index of category object.
   * @param {string} objectName               - Name of desired object.
   * @param {string} propertyName             - Name of desired property.
   * @param {T} defaultValue                  - Default value of desired property.
   */
  export function getCategoricalObjectValue<T>(category: DataViewCategoryColumn, index: number, objectName: string, propertyName: string, defaultValue: T): T {
      let categoryObjects = category.objects;

      if (categoryObjects) {
          let categoryObject: DataViewObject = categoryObjects[index];
          if (categoryObject) {
              let object = categoryObject[objectName];
              if (object) {
                  let property: T = <T>object[propertyName];
                  if (property !== undefined) {
                      return property;
                  }
              }
          }
      }
      return defaultValue;
  }



   export class VisualSettings extends DataViewObjectsParser {
     // public rcv_script: rcv_scriptSettings = new rcv_scriptSettings();
     public settings_thresholds_params: settings_thresholds_params = new settings_thresholds_params();
     public settings_rules_params: settings_rules_params = new settings_rules_params();
     public settings_viz_params: settings_viz_params = new settings_viz_params();
     public settings_additional_params: settings_additional_params = new settings_additional_params();
     
      }

    // export class rcv_scriptSettings {
    //  // undefined
    //   public provider     // undefined
    //   public source     }
    export class settings_thresholds_params {
      public  minRuleLength: string =  "2";
      public maxRuleLength: string =  "8";
      public threshSupport: number =  0.01;
      public threshConfidence: number =  0.6;
      public threshLift: number =  1.1;
    }
    export class settings_rules_params {
      public sortBy: string =  "lift";
      public showFrom: number =  1;
      public showTo: number =  5;
    }
    export class settings_viz_params {
      public visualisationMethod: string =  "graph";
      public rulesPerPlate: string =  "1";
      public textSize: number =  10;
      public edgeCol: string =  "green";
      public labelCol: string =  "orange";
      public colorBy: string =  "lift";
    }
    export class settings_additional_params {
      public showWarnings: boolean =  false
    }

}
