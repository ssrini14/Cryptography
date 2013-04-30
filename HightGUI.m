function varargout = HightGUI(varargin)
% HIGHTGUI MATLAB code for HightGUI.fig
%      HIGHTGUI, by itself, creates a new HIGHTGUI or raises the existing
%      singleton*.
%
%      H = HIGHTGUI returns the handle to a new HIGHTGUI or the handle to
%      the existing singleton*.
%
%      HIGHTGUI('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in HIGHTGUI.M with the given input arguments.
%
%      HIGHTGUI('Property','Value',...) creates a new HIGHTGUI or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before HightGUI_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to HightGUI_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help HightGUI

% Last Modified by GUIDE v2.5 10-Apr-2013 17:56:22

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
    'gui_Singleton',  gui_Singleton, ...
    'gui_OpeningFcn', @HightGUI_OpeningFcn, ...
    'gui_OutputFcn',  @HightGUI_OutputFcn, ...
    'gui_LayoutFcn',  [] , ...
    'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before HightGUI is made visible.
function HightGUI_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to HightGUI (see VARARGIN)

% Choose default command line output for HightGUI
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes HightGUI wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = HightGUI_OutputFcn(hObject, eventdata, handles)
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;



function PlainText_Callback(hObject, eventdata, handles)
% hObject    handle to PlainText (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of PlainText as text
%        str2double(get(hObject,'String')) returns contents of PlainText as a double
PTHex = get(hObject,'String');
capExpr = '[^0123456789ABCDEFabcdef\s]';
capStartIndex = regexp(PTHex,capExpr, 'once');

if(~isempty(capStartIndex))
    h = msgbox('Incorrect Hexadecimal Format','Plain/Cipher Text','error');
    set(hObject,'String','');
else
    PTHex(isspace(PTHex)) = [];
    PTHexLen = length(PTHex);
    if(PTHexLen < 1 || PTHexLen > 16)
        h = msgbox('Plain text or Cipher text must be 64 Bits','Plain/Cipher Text','error');
        set(hObject,'String','');
    else
        PTHex = upper(PTHex);
        set(hObject,'String',PTHex);
    end
    
end




% --- Executes during object creation, after setting all properties.
function PlainText_CreateFcn(hObject, eventdata, handles)
% hObject    handle to PlainText (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end



function KeyText_Callback(hObject, eventdata, handles)
% hObject    handle to KeyText (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of KeyText as text
%        str2double(get(hObject,'String')) returns contents of KeyText as a double
KeyHex = get(hObject,'String');
capExpr = '[^0123456789ABCDEFabcdef\s]';
capStartIndex = regexp(KeyHex,capExpr, 'once');
if(~isempty(capStartIndex))
    h = msgbox('Incorrect Hexadecimal Format','Key Text','error');
    set(hObject,'String','');
else
    KeyHex(isspace(KeyHex)) = [];
    KeyHexLen = length(KeyHex);
    if(KeyHexLen < 1 || KeyHexLen > 32)
        h = msgbox('Key text must be 128 Bits','Key Text','error');
        set(hObject,'String','');
    else
        KeyHex = upper(KeyHex);
        set(hObject,'String',KeyHex);
    end
    
end



% --- Executes during object creation, after setting all properties.
function KeyText_CreateFcn(hObject, eventdata, handles)
% hObject    handle to KeyText (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in HightOperBtn.
function HightOperBtn_Callback(hObject, eventdata, handles)
% hObject    handle to HightOperBtn (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
valueEncryptRB = get(handles.EncryptRB,'Value');
if(valueEncryptRB)
    operation = 'encrypt';
else
    operation = 'decrypt';
end
PTHex = get(handles.PlainText,'String');
PT = hex2dec(reshape(PTHex,2,[]).');
KeyHex = get(handles.KeyText,'String');
Key = hex2dec(reshape(KeyHex,2,[]).');
round = str2double(get(handles.RoundText,'String'));
Hgt = Hight();
Hgt.setRounds(round);
Hgt.setKey(Key);
[CT,CTHex,inputPT,PTHex] = Hgt.encrypt(PT);
%[ CTHex ] = Hight(PTHex, KeyHex, round,operation);
set(handles.outputText,'String',CTHex);


function RoundText_Callback(hObject, eventdata, handles)
% hObject    handle to RoundText (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of RoundText as text
%        str2double(get(hObject,'String')) returns contents of RoundText as a double
roundStr = get(hObject,'String');
capExpr = '[^0123456789]';
capStartIndex = regexp(roundStr,capExpr, 'once');
if(~isempty(capStartIndex))
    h = msgbox('Round value must be an Integer','Round Value','error');
    set(hObject,'String','32');
else
    round = str2double(get(hObject,'String'));
    if(round < 1 || round > 32)
        h = msgbox('Round of Hight must be 1-32','Round Value','error');
        set(hObject,'String','32');
    end
end


% --- Executes during object creation, after setting all properties.
function RoundText_CreateFcn(hObject, eventdata, handles)
% hObject    handle to RoundText (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in EncryptRB.
function EncryptRB_Callback(hObject, eventdata, handles)
% hObject    handle to EncryptRB (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
valueEncryptRB = get(hObject,'Value');
if(valueEncryptRB)
    set(handles.DecryptRB,'Value',0);
    set(handles.PTlabel,'String','Plain Text (Hexadecimal)');
    set(handles.Cipherlabel,'String','Cipher Text (Hexadecimal)');
end
% Hint: get(hObject,'Value') returns toggle state of EncryptRB


% --- Executes on button press in DecryptRB.
function DecryptRB_Callback(hObject, eventdata, handles)
% hObject    handle to DecryptRB (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: get(hObject,'Value') returns toggle state of DecryptRB
valueDecryptRB = get(hObject,'Value');
if(valueDecryptRB)
    set(handles.EncryptRB,'Value',0);
    set(handles.PTlabel,'String','Cipher Text (Hexadecimal)');
    set(handles.Cipherlabel,'String','Plain Text (Hexadecimal)');
end
