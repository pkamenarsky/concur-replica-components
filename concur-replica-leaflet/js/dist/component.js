"use strict";
const registerComponent = window['registerComponent'];
registerComponent("leaflet", (sendEvent) => {
    return {
        patchChild: (domNode, diff) => {
            console.log(domNode, diff);
        },
        patchAttribute: (domNode, diff) => {
            console.log(domNode, diff);
        }
    };
});
