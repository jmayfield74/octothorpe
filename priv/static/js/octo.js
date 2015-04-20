// throw together some oldskool/dirty js and css, but make it work.

var websocket;
var GAMEBOARD_NEW = [null, null, null, null, null, null, null, null, null];
var gameboard = GAMEBOARD_NEW;
var wintypes = {1: [1,2,3],
                2: [4,5,6],
                3: [7,8,9],
                4: [1,4,7],
                5: [2,5,8],
                6: [3,6,9],
                7: [1,5,9],
                8: [3,5,7],
                0: []};

function init_ws() {
    initWs();
    initBoard();
};


function init_rest() {
    initRest();
    initBoard();
};

function initBoard() {
    $("#controls").hide();
    $("#controls").removeClass("over");
    $("#board").children().each(function (idx, el) {
        var elem = $(el);
        var childr = el.children[0];
        var child = $(childr);
        child.fadeOut(300, "swing", function () {
            child.html("");
            elem.removeClass("off");
            elem.removeClass("win");
        });
        elem.click(function () {makeMove(childr.id);});
    });
    showMessage("");
};

function initWs() {
    websocket = new WebSocket("ws://" + window.location.host + "/ws");
    websocket.onmessage = function(evt) { onMessage(evt); };
};

function initRest() {
};

function disconnect() {
    websocket.close();
};

function makeMove(Id) {
    if ($("#"+Id).html() == "") {
        switch (websocket) {
        case undefined:
            restMove(Id);
            break;
        default:
            wsMove(Id);
            break;
        };
    }
};

function wsMove(Id) {
    if(websocket.readyState == websocket.OPEN){
        websocket.send(payload(Id, gameboard));
    } else {
        showMessage('websocket is not connected');
    };
};

function restMove(Id) {
    var url = "http://" + window.location.host + "/rest/play";
    $.post(url, payload(Id, gameboard), restResponder, "json");
}

function onMessage(evt) {
    updateScreen($.parseJSON(evt.data));
};

function restResponder(Data) {
    updateScreen(Data);
}

function showMessage(txt) {
    if (txt !== undefined) {
    $("#message").html(txt);
    }
};

function updateScreen(Data) {
    var response = Data[1];
    gameboard = response.board;
    $.each(response.board, function (idx, el) {
        var elem = $("#pos_" + (idx + 1));
        if (elem.html() === "") {
        elem.hide();
        elem.html(el);
        elem.fadeIn({duration: 150});
        }
    });
    if (response.status !== undefined) {
        game_over(response);
    }
};

function game_over(Data) {
    $("#controls").addClass("over");
    $("#controls").fadeIn();
    var wins = elemsForWin(Data.winner.wintype);
    $("#board").children().each(function (idx, el) {
        var elem = $(el);
        elem.off("click");
        elem.addClass("off");
    });
    $.each(elemsForWin(Data.winner.wintype), function (idx, el) {
        var elem = $("#pos_" + el);
        var par = elem.parent();
        par.removeClass("off");
        par.addClass("win");
    });
    showMessage(Data.winner.who + " wins!");
}

function startOver() {
    gameboard = GAMEBOARD_NEW;
    initBoard();
}

function payload(Id, Board) {
    var pos = Id.split("_")[1];
    return JSON.stringify({"board": Board, "pos": pos});
};

function elemsForWin(wintype) {
    return wintypes[wintype];
};
