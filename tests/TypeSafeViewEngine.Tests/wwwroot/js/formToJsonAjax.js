document.getElementById("form").onsubmit = submitAsJson

function submitAsJson(evt) {
    evt.preventDefault();
    // var json = formToObject(this) || {};
    var json = $(this).serializeJSON({useIntKeysAsArrayIndex : true})
    console.log(json)
    fetch("/",{
        method: "POST",
        headers: {
            'Content-Type' : 'application/json'
        },
        body: JSON.stringify(json)
    })
}
