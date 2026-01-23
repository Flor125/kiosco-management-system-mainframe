const quaggaConf = {
    inputStream: {
        target: document.querySelector("#camera"),
        type: "LiveStream",
        constraints: {
            width: {min: 640},
            height: {min: 480},
            facingMode: "environment",
            aspectRatio: {min: 1, max: 2}
        },
    },
    decoder: {
        readers: ["code_128_reader"]
    },
}

Quagga.init(quaggaConf, function(err) {
    if (err) {
        return console.log(err);
    }
    Quagga.start();
});

Quagga.onDetected(function(result){
    alert("Código de barras detectado: " + result.codeResult.code);
});