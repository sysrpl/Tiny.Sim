unit Tiny.Text.Glyphs;

{$i tiny.inc}

interface

{ Convert a unicode point to a string }

function UnicodeToStr(C: LongWord): string;

const
  MaterialGlyphs: array of string = [
    'vector-square $F0001', 'access-point-network $F0002', 'access-point $F0003',
    'account $F0004', 'account-alert $F0005', 'account-box $F0006',
    'account-box-outline $F0007', 'account-check $F0008', 'account-circle $F0009',
    'account-convert $F000A', 'account-key $F000B', 'tooltip-account $F000C',
    'account-minus $F000D', 'account-multiple $F000E', 'account-multiple-outline $F000F',
    'account-multiple-plus $F0010', 'account-network $F0011', 'account-off $F0012',
    'account-outline $F0013', 'account-plus $F0014', 'account-remove $F0015',
    'account-search $F0016', 'account-star $F0017', 'orbit $F0018',
    'account-switch $F0019', 'adjust $F001A', 'air-conditioner $F001B',
    'airballoon $F001C', 'airplane $F001D', 'airplane-off $F001E',
    'cast-variant $F001F', 'alarm $F0020', 'alarm-check $F0021',
    'alarm-multiple $F0022', 'alarm-off $F0023', 'alarm-plus $F0024',
    'album $F0025', 'alert $F0026', 'alert-box $F0027',
    'alert-circle $F0028', 'alert-octagon $F0029', 'alert-outline $F002A',
    'alpha $F002B', 'alphabetical $F002C', 'greenhouse $F002D',
    'rollerblade-off $F002E', 'ambulance $F002F', 'amplifier $F0030',
    'anchor $F0031', 'android $F0032', 'web-plus $F0033',
    'android-studio $F0034', 'apple $F0035', 'apple-finder $F0036',
    'apple-ios $F0037', 'apple-icloud $F0038', 'apple-safari $F0039',
    'font-awesome $F003A', 'apps $F003B', 'archive $F003C',
    'arrange-bring-forward $F003D', 'arrange-bring-to-front $F003E', 'arrange-send-backward $F003F',
    'arrange-send-to-back $F0040', 'arrow-all $F0041', 'arrow-bottom-left $F0042',
    'arrow-bottom-right $F0043', 'arrow-collapse-all $F0044', 'arrow-down $F0045',
    'arrow-down-thick $F0046', 'arrow-down-bold-circle $F0047', 'arrow-down-bold-circle-outline $F0048',
    'arrow-down-bold-hexagon-outline $F0049', 'arrow-down-drop-circle $F004A', 'arrow-down-drop-circle-outline $F004B',
    'arrow-expand-all $F004C', 'arrow-left $F004D', 'arrow-left-thick $F004E',
    'arrow-left-bold-circle $F004F', 'arrow-left-bold-circle-outline $F0050', 'arrow-left-bold-hexagon-outline $F0051',
    'arrow-left-drop-circle $F0052', 'arrow-left-drop-circle-outline $F0053', 'arrow-right $F0054',
    'arrow-right-thick $F0055', 'arrow-right-bold-circle $F0056', 'arrow-right-bold-circle-outline $F0057',
    'arrow-right-bold-hexagon-outline $F0058', 'arrow-right-drop-circle $F0059', 'arrow-right-drop-circle-outline $F005A',
    'arrow-top-left $F005B', 'arrow-top-right $F005C', 'arrow-up $F005D',
    'arrow-up-thick $F005E', 'arrow-up-bold-circle $F005F', 'arrow-up-bold-circle-outline $F0060',
    'arrow-up-bold-hexagon-outline $F0061', 'arrow-up-drop-circle $F0062', 'arrow-up-drop-circle-outline $F0063',
    'assistant $F0064', 'at $F0065', 'attachment $F0066',
    'book-music $F0067', 'auto-fix $F0068', 'auto-upload $F0069',
    'autorenew $F006A', 'av-timer $F006B', 'baby $F006C',
    'backburger $F006D', 'backspace $F006E', 'backup-restore $F006F',
    'bank $F0070', 'barcode $F0071', 'barcode-scan $F0072',
    'barley $F0073', 'barrel $F0074', 'incognito-off $F0075',
    'basket $F0076', 'basket-fill $F0077', 'basket-unfill $F0078',
    'battery $F0079', 'battery-10 $F007A', 'battery-20 $F007B',
    'battery-30 $F007C', 'battery-40 $F007D', 'battery-50 $F007E',
    'battery-60 $F007F', 'battery-70 $F0080', 'battery-80 $F0081',
    'battery-90 $F0082', 'battery-alert $F0083', 'battery-charging $F0084',
    'battery-charging-100 $F0085', 'battery-charging-20 $F0086', 'battery-charging-30 $F0087',
    'battery-charging-40 $F0088', 'battery-charging-60 $F0089', 'battery-charging-80 $F008A',
    'battery-charging-90 $F008B', 'battery-minus-variant $F008C', 'battery-negative $F008D',
    'battery-outline $F008E', 'battery-plus-variant $F008F', 'battery-positive $F0090',
    'battery-unknown $F0091', 'beach $F0092', 'flask $F0093',
    'flask-empty $F0094', 'flask-empty-outline $F0095', 'flask-outline $F0096',
    'bunk-bed-outline $F0097', 'beer $F0098', 'bed-outline $F0099',
    'bell $F009A', 'bell-off $F009B', 'bell-outline $F009C',
    'bell-plus $F009D', 'bell-ring $F009E', 'bell-ring-outline $F009F',
    'bell-sleep $F00A0', 'beta $F00A1', 'book-cross $F00A2',
    'bike $F00A3', 'microsoft-bing $F00A4', 'binoculars $F00A5',
    'bio $F00A6', 'biohazard $F00A7', 'bitbucket $F00A8',
    'black-mesa $F00A9', 'shield-refresh $F00AA', 'blender-software $F00AB',
    'blinds $F00AC', 'block-helper $F00AD', 'application-edit $F00AE',
    'bluetooth $F00AF', 'bluetooth-audio $F00B0', 'bluetooth-connect $F00B1',
    'bluetooth-off $F00B2', 'bluetooth-settings $F00B3', 'bluetooth-transfer $F00B4',
    'blur $F00B5', 'blur-linear $F00B6', 'blur-off $F00B7',
    'blur-radial $F00B8', 'bone $F00B9', 'book $F00BA',
    'book-multiple $F00BB', 'book-variant-multiple $F00BC', 'book-open $F00BD',
    'book-open-blank-variant $F00BE', 'book-variant $F00BF', 'bookmark $F00C0',
    'bookmark-check $F00C1', 'bookmark-music $F00C2', 'bookmark-outline $F00C3',
    'bookmark-plus-outline $F00C4', 'bookmark-plus $F00C5', 'bookmark-remove $F00C6',
    'border-all $F00C7', 'border-bottom $F00C8', 'border-color $F00C9',
    'border-horizontal $F00CA', 'border-inside $F00CB', 'border-left $F00CC',
    'border-none $F00CD', 'border-outside $F00CE', 'border-right $F00CF',
    'border-style $F00D0', 'border-top $F00D1', 'border-vertical $F00D2',
    'bowling $F00D3', 'box $F00D4', 'box-cutter $F00D5',
    'briefcase $F00D6', 'briefcase-check $F00D7', 'briefcase-download $F00D8',
    'briefcase-upload $F00D9', 'brightness-1 $F00DA', 'brightness-2 $F00DB',
    'brightness-3 $F00DC', 'brightness-4 $F00DD', 'brightness-5 $F00DE',
    'brightness-6 $F00DF', 'brightness-7 $F00E0', 'brightness-auto $F00E1',
    'broom $F00E2', 'brush $F00E3', 'bug $F00E4',
    'bulletin-board $F00E5', 'bullhorn $F00E6', 'bus $F00E7',
    'cached $F00E8', 'cake $F00E9', 'cake-layered $F00EA',
    'cake-variant $F00EB', 'calculator $F00EC', 'calendar $F00ED',
    'calendar-blank $F00EE', 'calendar-check $F00EF', 'calendar-clock $F00F0',
    'calendar-multiple $F00F1', 'calendar-multiple-check $F00F2', 'calendar-plus $F00F3',
    'calendar-remove $F00F4', 'calendar-text $F00F5', 'calendar-today $F00F6',
    'call-made $F00F7', 'call-merge $F00F8', 'call-missed $F00F9',
    'call-received $F00FA', 'call-split $F00FB', 'camcorder $F00FC',
    'video-box $F00FD', 'video-box-off $F00FE', 'camcorder-off $F00FF',
    'camera $F0100', 'camera-enhance $F0101', 'camera-front $F0102',
    'camera-front-variant $F0103', 'camera-iris $F0104', 'camera-party-mode $F0105',
    'camera-rear $F0106', 'camera-rear-variant $F0107', 'camera-switch $F0108',
    'camera-timer $F0109', 'candycane $F010A', 'car $F010B',
    'car-battery $F010C', 'car-connected $F010D', 'car-wash $F010E',
    'carrot $F010F', 'cart $F0110', 'cart-outline $F0111',
    'cart-plus $F0112', 'case-sensitive-alt $F0113', 'cash $F0114',
    'cash-100 $F0115', 'cash-multiple $F0116', 'checkbox-blank-badge-outline $F0117',
    'cast $F0118', 'cast-connected $F0119', 'castle $F011A',
    'cat $F011B', 'cellphone $F011C', 'tray-arrow-up $F011D',
    'cellphone-basic $F011E', 'cellphone-dock $F011F', 'tray-arrow-down $F0120',
    'cellphone-link $F0121', 'cellphone-link-off $F0122', 'cellphone-settings $F0123',
    'certificate $F0124', 'chair-school $F0125', 'chart-arc $F0126',
    'chart-areaspline $F0127', 'chart-bar $F0128', 'chart-histogram $F0129',
    'chart-line $F012A', 'chart-pie $F012B', 'check $F012C',
    'check-all $F012D', 'checkbox-blank $F012E', 'checkbox-blank-circle $F012F',
    'checkbox-blank-circle-outline $F0130', 'checkbox-blank-outline $F0131', 'checkbox-marked $F0132',
    'checkbox-marked-circle $F0133', 'checkbox-marked-circle-outline $F0134', 'checkbox-marked-outline $F0135',
    'checkbox-multiple-blank $F0136', 'checkbox-multiple-blank-outline $F0137', 'checkbox-multiple-marked $F0138',
    'checkbox-multiple-marked-outline $F0139', 'checkerboard $F013A', 'chemical-weapon $F013B',
    'chevron-double-down $F013C', 'chevron-double-left $F013D', 'chevron-double-right $F013E',
    'chevron-double-up $F013F', 'chevron-down $F0140', 'chevron-left $F0141',
    'chevron-right $F0142', 'chevron-up $F0143', 'church $F0144',
    'roller-skate-off $F0145', 'city $F0146', 'clipboard $F0147',
    'clipboard-account $F0148', 'clipboard-alert $F0149', 'clipboard-arrow-down $F014A',
    'clipboard-arrow-left $F014B', 'clipboard-outline $F014C', 'clipboard-text $F014D',
    'clipboard-check $F014E', 'clippy $F014F', 'clock-outline $F0150',
    'clock-end $F0151', 'clock-fast $F0152', 'clock-in $F0153',
    'clock-out $F0154', 'clock-start $F0155', 'close $F0156',
    'close-box $F0157', 'close-box-outline $F0158', 'close-circle $F0159',
    'close-circle-outline $F015A', 'close-network $F015B', 'close-octagon $F015C',
    'close-octagon-outline $F015D', 'closed-caption $F015E', 'cloud $F015F',
    'cloud-check $F0160', 'cloud-circle $F0161', 'cloud-download $F0162',
    'cloud-outline $F0163', 'cloud-off-outline $F0164', 'cloud-print $F0165',
    'cloud-print-outline $F0166', 'cloud-upload $F0167', 'code-array $F0168',
    'code-braces $F0169', 'code-brackets $F016A', 'code-equal $F016B',
    'code-greater-than $F016C', 'code-greater-than-or-equal $F016D', 'code-less-than $F016E',
    'code-less-than-or-equal $F016F', 'code-not-equal $F0170', 'code-not-equal-variant $F0171',
    'code-parentheses $F0172', 'code-string $F0173', 'code-tags $F0174',
    'codepen $F0175', 'coffee $F0176', 'coffee-to-go $F0177',
    'bell-badge-outline $F0178', 'color-helper $F0179', 'comment $F017A',
    'comment-account $F017B', 'comment-account-outline $F017C', 'comment-alert $F017D',
    'comment-alert-outline $F017E', 'comment-check $F017F', 'comment-check-outline $F0180',
    'comment-multiple-outline $F0181', 'comment-outline $F0182', 'comment-plus-outline $F0183',
    'comment-processing $F0184', 'comment-processing-outline $F0185', 'comment-question-outline $F0186',
    'comment-remove-outline $F0187', 'comment-text $F0188', 'comment-text-outline $F0189',
    'compare $F018A', 'compass $F018B', 'compass-outline $F018C',
    'console $F018D', 'card-account-mail $F018E', 'content-copy $F018F',
    'content-cut $F0190', 'content-duplicate $F0191', 'content-paste $F0192',
    'content-save $F0193', 'content-save-all $F0194', 'contrast $F0195',
    'contrast-box $F0196', 'contrast-circle $F0197', 'cookie $F0198',
    'counter $F0199', 'cow $F019A', 'credit-card-outline $F019B',
    'credit-card-multiple-outline $F019C', 'credit-card-scan-outline $F019D', 'crop $F019E',
    'crop-free $F019F', 'crop-landscape $F01A0', 'crop-portrait $F01A1',
    'crop-square $F01A2', 'crosshairs $F01A3', 'crosshairs-gps $F01A4',
    'crown $F01A5', 'cube $F01A6', 'cube-outline $F01A7',
    'cube-send $F01A8', 'cube-unfolded $F01A9', 'cup $F01AA',
    'cup-water $F01AB', 'currency-btc $F01AC', 'currency-eur $F01AD',
    'currency-gbp $F01AE', 'currency-inr $F01AF', 'currency-ngn $F01B0',
    'currency-rub $F01B1', 'currency-try $F01B2', 'delete-variant $F01B3',
    'delete $F01B4', 'decimal-increase $F01B5', 'decimal-decrease $F01B6',
    'debug-step-over $F01B7', 'debug-step-out $F01B8', 'debug-step-into $F01B9',
    'database-plus $F01BA', 'database-minus $F01BB', 'database $F01BC',
    'cursor-pointer $F01BD', 'cursor-move $F01BE', 'cursor-default-outline $F01BF',
    'cursor-default $F01C0', 'currency-usd $F01C1', 'delta $F01C2',
    'deskphone $F01C3', 'desktop-mac $F01C4', 'desktop-tower $F01C5',
    'details $F01C6', 'deviantart $F01C7', 'diamond-stone $F01C8',
    'ab-testing $F01C9', 'dice-1 $F01CA', 'dice-2 $F01CB',
    'dice-3 $F01CC', 'dice-4 $F01CD', 'dice-5 $F01CE',
    'dice-6 $F01CF', 'directions $F01D0', 'disc-alert $F01D1',
    'disqus $F01D2', 'video-plus-outline $F01D3', 'division $F01D4',
    'division-box $F01D5', 'dns $F01D6', 'domain $F01D7',
    'dots-horizontal $F01D8', 'dots-vertical $F01D9', 'download $F01DA',
    'drag $F01DB', 'drag-horizontal $F01DC', 'drag-vertical $F01DD',
    'drawing $F01DE', 'drawing-box $F01DF', 'shield-refresh-outline $F01E0',
    'calendar-refresh $F01E1', 'drone $F01E2', 'dropbox $F01E3',
    'drupal $F01E4', 'duck $F01E5', 'dumbbell $F01E6',
    'earth $F01E7', 'earth-off $F01E8', 'microsoft-edge $F01E9',
    'eject $F01EA', 'elevation-decline $F01EB', 'elevation-rise $F01EC',
    'elevator $F01ED', 'email $F01EE', 'email-open $F01EF',
    'email-outline $F01F0', 'email-lock $F01F1', 'emoticon-outline $F01F2',
    'emoticon-cool-outline $F01F3', 'emoticon-devil-outline $F01F4', 'emoticon-happy-outline $F01F5',
    'emoticon-neutral-outline $F01F6', 'emoticon-poop $F01F7', 'emoticon-sad-outline $F01F8',
    'emoticon-tongue $F01F9', 'engine $F01FA', 'engine-outline $F01FB',
    'equal $F01FC', 'equal-box $F01FD', 'eraser $F01FE',
    'escalator $F01FF', 'ethernet $F0200', 'ethernet-cable $F0201',
    'ethernet-cable-off $F0202', 'calendar-refresh-outline $F0203', 'evernote $F0204',
    'exclamation $F0205', 'exit-to-app $F0206', 'export $F0207',
    'eye $F0208', 'eye-off $F0209', 'eyedropper $F020A',
    'eyedropper-variant $F020B', 'facebook $F020C', 'order-alphabetical-ascending $F020D',
    'facebook-messenger $F020E', 'factory $F020F', 'fan $F0210',
    'fast-forward $F0211', 'fax $F0212', 'ferry $F0213',
    'file $F0214', 'file-chart $F0215', 'file-check $F0216',
    'file-cloud $F0217', 'file-delimited $F0218', 'file-document $F0219',
    'text-box $F021A', 'file-excel $F021B', 'file-excel-box $F021C',
    'file-export $F021D', 'file-find $F021E', 'file-image $F021F',
    'file-import $F0220', 'file-lock $F0221', 'file-multiple $F0222',
    'file-music $F0223', 'file-outline $F0224', 'file-jpg-box $F0225',
    'file-pdf-box $F0226', 'file-powerpoint $F0227', 'file-powerpoint-box $F0228',
    'file-presentation-box $F0229', 'file-send $F022A', 'file-video $F022B',
    'file-word $F022C', 'file-word-box $F022D', 'file-code $F022E',
    'film $F022F', 'filmstrip $F0230', 'filmstrip-off $F0231',
    'filter $F0232', 'filter-outline $F0233', 'filter-remove $F0234',
    'filter-remove-outline $F0235', 'filter-variant $F0236', 'fingerprint $F0237',
    'fire $F0238', 'firefox $F0239', 'fish $F023A',
    'flag $F023B', 'flag-checkered $F023C', 'flag-outline $F023D',
    'flag-variant-outline $F023E', 'flag-triangle $F023F', 'flag-variant $F0240',
    'flash $F0241', 'flash-auto $F0242', 'flash-off $F0243',
    'flashlight $F0244', 'flashlight-off $F0245', 'star-half $F0246',
    'flip-to-back $F0247', 'flip-to-front $F0248', 'floppy $F0249',
    'flower $F024A', 'folder $F024B', 'folder-account $F024C',
    'folder-download $F024D', 'folder-google-drive $F024E', 'folder-image $F024F',
    'folder-lock $F0250', 'folder-lock-open $F0251', 'folder-move $F0252',
    'folder-multiple $F0253', 'folder-multiple-image $F0254', 'folder-multiple-outline $F0255',
    'folder-outline $F0256', 'folder-plus $F0257', 'folder-remove $F0258',
    'folder-upload $F0259', 'food $F025A', 'food-apple $F025B',
    'food-variant $F025C', 'football $F025D', 'football-australian $F025E',
    'football-helmet $F025F', 'format-align-center $F0260', 'format-align-justify $F0261',
    'format-align-left $F0262', 'format-align-right $F0263', 'format-bold $F0264',
    'format-clear $F0265', 'format-color-fill $F0266', 'format-float-center $F0267',
    'format-float-left $F0268', 'format-float-none $F0269', 'format-float-right $F026A',
    'format-header-1 $F026B', 'format-header-2 $F026C', 'format-header-3 $F026D',
    'format-header-4 $F026E', 'format-header-5 $F026F', 'format-header-6 $F0270',
    'format-header-decrease $F0271', 'format-header-equal $F0272', 'format-header-increase $F0273',
    'format-header-pound $F0274', 'format-indent-decrease $F0275', 'format-indent-increase $F0276',
    'format-italic $F0277', 'format-line-spacing $F0278', 'format-list-bulleted $F0279',
    'format-list-bulleted-type $F027A', 'format-list-numbered $F027B', 'format-paint $F027C',
    'format-paragraph $F027D', 'format-quote-close $F027E', 'format-size $F027F',
    'format-strikethrough $F0280', 'format-strikethrough-variant $F0281', 'format-subscript $F0282',
    'format-superscript $F0283', 'format-text $F0284', 'format-textdirection-l-to-r $F0285',
    'format-textdirection-r-to-l $F0286', 'format-underline $F0287', 'format-wrap-inline $F0288',
    'format-wrap-square $F0289', 'format-wrap-tight $F028A', 'format-wrap-top-bottom $F028B',
    'forum $F028C', 'forward $F028D', 'bowl $F028E',
    'fridge-outline $F028F', 'fridge $F0290', 'fridge-top $F0291',
    'fridge-bottom $F0292', 'fullscreen $F0293', 'fullscreen-exit $F0294',
    'function $F0295', 'gamepad $F0296', 'gamepad-variant $F0297',
    'gas-station $F0298', 'gate $F0299', 'gauge $F029A',
    'gavel $F029B', 'gender-female $F029C', 'gender-male $F029D',
    'gender-male-female $F029E', 'gender-transgender $F029F', 'ghost $F02A0',
    'gift-outline $F02A1', 'git $F02A2', 'card-account-details-star $F02A3',
    'github $F02A4', 'glass-flute $F02A5', 'glass-mug $F02A6',
    'glass-stange $F02A7', 'glass-tulip $F02A8', 'bowl-outline $F02A9',
    'glasses $F02AA', 'gmail $F02AB', 'gnome $F02AC',
    'google $F02AD', 'google-cardboard $F02AE', 'google-chrome $F02AF',
    'google-circles $F02B0', 'google-circles-communities $F02B1', 'google-circles-extended $F02B2',
    'google-circles-group $F02B3', 'google-controller $F02B4', 'google-controller-off $F02B5',
    'google-drive $F02B6', 'google-earth $F02B7', 'google-glass $F02B8',
    'google-nearby $F02B9', 'video-minus-outline $F02BA', 'microsoft-teams $F02BB',
    'google-play $F02BC', 'google-plus $F02BD', 'order-bool-ascending $F02BE',
    'google-translate $F02BF', 'google-classroom $F02C0', 'grid $F02C1',
    'grid-off $F02C2', 'group $F02C3', 'guitar-electric $F02C4',
    'guitar-pick $F02C5', 'guitar-pick-outline $F02C6', 'hand-pointing-right $F02C7',
    'hanger $F02C8', 'google-hangouts $F02C9', 'harddisk $F02CA',
    'headphones $F02CB', 'headphones-box $F02CC', 'headphones-settings $F02CD',
    'headset $F02CE', 'headset-dock $F02CF', 'headset-off $F02D0',
    'heart $F02D1', 'heart-box $F02D2', 'heart-box-outline $F02D3',
    'heart-broken $F02D4', 'heart-outline $F02D5', 'help $F02D6',
    'help-circle $F02D7', 'hexagon $F02D8', 'hexagon-outline $F02D9',
    'history $F02DA', 'hololens $F02DB', 'home $F02DC',
    'home-modern $F02DD', 'home-variant $F02DE', 'hops $F02DF',
    'hospital-box $F02E0', 'hospital-building $F02E1', 'hospital-marker $F02E2',
    'bed $F02E3', 'bowl-mix-outline $F02E4', 'pot $F02E5',
    'human $F02E6', 'human-child $F02E7', 'human-male-female $F02E8',
    'image $F02E9', 'image-album $F02EA', 'image-area $F02EB',
    'image-area-close $F02EC', 'image-broken $F02ED', 'image-broken-variant $F02EE',
    'image-multiple-outline $F02EF', 'image-filter-black-white $F02F0', 'image-filter-center-focus $F02F1',
    'image-filter-center-focus-weak $F02F2', 'image-filter-drama $F02F3', 'image-filter-frames $F02F4',
    'image-filter-hdr $F02F5', 'image-filter-none $F02F6', 'image-filter-tilt-shift $F02F7',
    'image-filter-vintage $F02F8', 'image-multiple $F02F9', 'import $F02FA',
    'inbox-arrow-down $F02FB', 'information $F02FC', 'information-outline $F02FD',
    'instagram $F02FE', 'pot-outline $F02FF', 'microsoft-internet-explorer $F0300',
    'invert-colors $F0301', 'jeepney $F0302', 'jira $F0303',
    'jsfiddle $F0304', 'keg $F0305', 'key $F0306',
    'key-change $F0307', 'key-minus $F0308', 'key-plus $F0309',
    'key-remove $F030A', 'key-variant $F030B', 'keyboard $F030C',
    'keyboard-backspace $F030D', 'keyboard-caps $F030E', 'keyboard-close $F030F',
    'keyboard-off $F0310', 'keyboard-return $F0311', 'keyboard-tab $F0312',
    'keyboard-variant $F0313', 'kodi $F0314', 'label $F0315',
    'label-outline $F0316', 'lan $F0317', 'lan-connect $F0318',
    'lan-disconnect $F0319', 'lan-pending $F031A', 'language-csharp $F031B',
    'language-css3 $F031C', 'language-html5 $F031D', 'language-javascript $F031E',
    'language-php $F031F', 'language-python $F0320', 'contactless-payment-circle $F0321',
    'laptop $F0322', 'magazine-rifle $F0323', 'magazine-pistol $F0324',
    'keyboard-tab-reverse $F0325', 'pot-steam-outline $F0326', 'launch $F0327',
    'layers $F0328', 'layers-off $F0329', 'leaf $F032A',
    'led-off $F032B', 'led-on $F032C', 'led-outline $F032D',
    'led-variant-off $F032E', 'led-variant-on $F032F', 'led-variant-outline $F0330',
    'library $F0331', 'filmstrip-box $F0332', 'music-box-multiple $F0333',
    'plus-box-multiple $F0334', 'lightbulb $F0335', 'lightbulb-outline $F0336',
    'link $F0337', 'link-off $F0338', 'link-variant $F0339',
    'link-variant-off $F033A', 'linkedin $F033B', 'sort-reverse-variant $F033C',
    'linux $F033D', 'lock $F033E', 'lock-open $F033F',
    'lock-open-outline $F0340', 'lock-outline $F0341', 'login $F0342',
    'logout $F0343', 'looks $F0344', 'loupe $F0345',
    'lumx $F0346', 'magnet $F0347', 'magnet-on $F0348',
    'magnify $F0349', 'magnify-minus $F034A', 'magnify-plus $F034B',
    'plus-circle-multiple $F034C', 'map $F034D', 'map-marker $F034E',
    'map-marker-circle $F034F', 'map-marker-multiple $F0350', 'map-marker-off $F0351',
    'map-marker-radius $F0352', 'margin $F0353', 'language-markdown $F0354',
    'marker-check $F0355', 'glass-cocktail $F0356', 'material-ui $F0357',
    'math-compass $F0358', 'stackpath $F0359', 'minus-circle-multiple $F035A',
    'memory $F035B', 'menu $F035C', 'menu-down $F035D',
    'menu-left $F035E', 'menu-right $F035F', 'menu-up $F0360',
    'message $F0361', 'message-alert $F0362', 'message-draw $F0363',
    'message-image $F0364', 'message-outline $F0365', 'message-processing $F0366',
    'message-reply $F0367', 'message-reply-text $F0368', 'message-text $F0369',
    'message-text-outline $F036A', 'message-video $F036B', 'microphone $F036C',
    'microphone-off $F036D', 'microphone-outline $F036E', 'microphone-settings $F036F',
    'microphone-variant $F0370', 'microphone-variant-off $F0371', 'microsoft $F0372',
    'minecraft $F0373', 'minus $F0374', 'minus-box $F0375',
    'minus-circle $F0376', 'minus-circle-outline $F0377', 'minus-network $F0378',
    'monitor $F0379', 'monitor-multiple $F037A', 'more $F037B',
    'motorbike $F037C', 'mouse $F037D', 'mouse-off $F037E',
    'mouse-variant $F037F', 'mouse-variant-off $F0380', 'movie $F0381',
    'multiplication $F0382', 'multiplication-box $F0383', 'music-box $F0384',
    'music-box-outline $F0385', 'music-circle $F0386', 'music-note $F0387',
    'music-note-half $F0389', 'music-note-off $F038A', 'music-note-quarter $F038B',
    'music-note-sixteenth $F038C', 'music-note-whole $F038D', 'nature $F038E',
    'nature-people $F038F', 'navigation $F0390', 'needle $F0391',
    'smoke-detector $F0392', 'thermostat $F0393', 'new-box $F0394',
    'newspaper $F0395', 'nfc $F0396', 'nfc-tap $F0397',
    'nfc-variant $F0398', 'nodejs $F0399', 'note $F039A',
    'note-outline $F039B', 'note-plus $F039C', 'note-plus-outline $F039D',
    'note-text $F039E', 'notification-clear-all $F039F', 'numeric $F03A0',
    'numeric-0-box $F03A1', 'numeric-0-box-multiple-outline $F03A2', 'numeric-0-box-outline $F03A3',
    'numeric-1-box $F03A4', 'numeric-1-box-multiple-outline $F03A5', 'numeric-1-box-outline $F03A6',
    'numeric-2-box $F03A7', 'numeric-2-box-multiple-outline $F03A8', 'numeric-2-box-outline $F03A9',
    'numeric-3-box $F03AA', 'numeric-3-box-multiple-outline $F03AB', 'numeric-3-box-outline $F03AC',
    'numeric-4-box $F03AD', 'numeric-4-box-outline $F03AE', 'numeric-5-box-multiple-outline $F03AF',
    'numeric-5-box-outline $F03B0', 'numeric-5-box $F03B1', 'numeric-4-box-multiple-outline $F03B2',
    'numeric-6-box $F03B3', 'numeric-6-box-multiple-outline $F03B4', 'numeric-6-box-outline $F03B5',
    'numeric-7-box $F03B6', 'numeric-7-box-multiple-outline $F03B7', 'numeric-7-box-outline $F03B8',
    'numeric-8-box $F03B9', 'numeric-8-box-multiple-outline $F03BA', 'numeric-8-box-outline $F03BB',
    'numeric-9-box $F03BC', 'numeric-9-box-multiple-outline $F03BD', 'numeric-9-box-outline $F03BE',
    'numeric-9-plus-box $F03BF', 'numeric-9-plus-box-multiple-outline $F03C0', 'numeric-9-plus-box-outline $F03C1',
    'nutrition $F03C2', 'octagon $F03C3', 'octagon-outline $F03C4',
    'odnoklassniki $F03C5', 'microsoft-office $F03C6', 'oil $F03C7',
    'coolant-temperature $F03C8', 'omega $F03C9', 'microsoft-onedrive $F03CA',
    'open-in-app $F03CB', 'open-in-new $F03CC', 'openid $F03CD',
    'opera $F03CE', 'ornament $F03CF', 'ornament-variant $F03D0',
    'inbox-arrow-up $F03D1', 'owl $F03D2', 'package $F03D3',
    'package-down $F03D4', 'package-up $F03D5', 'package-variant $F03D6',
    'package-variant-closed $F03D7', 'palette $F03D8', 'palette-advanced $F03D9',
    'panda $F03DA', 'pandora $F03DB', 'panorama $F03DC',
    'panorama-fisheye $F03DD', 'panorama-horizontal-outline $F03DE', 'panorama-vertical-outline $F03DF',
    'panorama-wide-angle-outline $F03E0', 'paper-cut-vertical $F03E1', 'paperclip $F03E2',
    'parking $F03E3', 'pause $F03E4', 'pause-circle $F03E5',
    'pause-circle-outline $F03E6', 'pause-octagon $F03E7', 'pause-octagon-outline $F03E8',
    'paw $F03E9', 'pen $F03EA', 'pencil $F03EB',
    'pencil-box $F03EC', 'pencil-box-outline $F03ED', 'pencil-lock $F03EE',
    'pencil-off $F03EF', 'percent $F03F0', 'mortar-pestle-plus $F03F1',
    'phone $F03F2', 'phone-bluetooth $F03F3', 'phone-forward $F03F4',
    'phone-hangup $F03F5', 'phone-in-talk $F03F6', 'phone-incoming $F03F7',
    'phone-lock $F03F8', 'phone-log $F03F9', 'phone-missed $F03FA',
    'phone-outgoing $F03FB', 'phone-paused $F03FC', 'phone-settings $F03FD',
    'phone-voip $F03FE', 'pi $F03FF', 'pi-box $F0400',
    'pig $F0401', 'pill $F0402', 'pin $F0403',
    'pin-off $F0404', 'pine-tree $F0405', 'pine-tree-box $F0406',
    'pinterest $F0407', 'contactless-payment-circle-outline $F0408', 'pizza $F0409',
    'play $F040A', 'play-box-outline $F040B', 'play-circle $F040C',
    'play-circle-outline $F040D', 'play-pause $F040E', 'play-protected-content $F040F',
    'playlist-minus $F0410', 'playlist-play $F0411', 'playlist-plus $F0412',
    'playlist-remove $F0413', 'sony-playstation $F0414', 'plus $F0415',
    'plus-box $F0416', 'plus-circle $F0417', 'plus-circle-multiple-outline $F0418',
    'plus-circle-outline $F0419', 'plus-network $F041A', 'sledding $F041B',
    'wall-sconce-flat-variant $F041C', 'pokeball $F041D', 'polaroid $F041E',
    'poll $F041F', 'account-eye $F0420', 'polymer $F0421',
    'popcorn $F0422', 'pound $F0423', 'pound-box $F0424',
    'power $F0425', 'power-settings $F0426', 'power-socket $F0427',
    'presentation $F0428', 'presentation-play $F0429', 'printer $F042A',
    'printer-3d $F042B', 'printer-alert $F042C', 'professional-hexagon $F042D',
    'projector $F042E', 'projector-screen $F042F', 'pulse $F0430',
    'puzzle $F0431', 'qrcode $F0432', 'qrcode-scan $F0433',
    'quadcopter $F0434', 'quality-high $F0435', 'book-multiple-outline $F0436',
    'radar $F0437', 'radiator $F0438', 'radio $F0439',
    'radio-handheld $F043A', 'radio-tower $F043B', 'radioactive $F043C',
    'radiobox-marked $F043E', 'raspberry-pi $F043F', 'ray-end $F0440',
    'ray-end-arrow $F0441', 'ray-start $F0442', 'ray-start-arrow $F0443',
    'ray-start-end $F0444', 'ray-vertex $F0445', 'lastpass $F0446',
    'read $F0447', 'youtube-tv $F0448', 'receipt $F0449',
    'record $F044A', 'record-rec $F044B', 'recycle $F044C',
    'reddit $F044D', 'redo $F044E', 'redo-variant $F044F',
    'refresh $F0450', 'regex $F0451', 'relative-scale $F0452',
    'reload $F0453', 'remote $F0454', 'rename-box $F0455',
    'repeat $F0456', 'repeat-off $F0457', 'repeat-once $F0458',
    'replay $F0459', 'reply $F045A', 'reply-all $F045B',
    'reproduction $F045C', 'resize-bottom-right $F045D', 'responsive $F045E',
    'rewind $F045F', 'ribbon $F0460', 'road $F0461',
    'road-variant $F0462', 'rocket $F0463', 'rotate-3d-variant $F0464',
    'rotate-left $F0465', 'rotate-left-variant $F0466', 'rotate-right $F0467',
    'rotate-right-variant $F0468', 'router-wireless $F0469', 'routes $F046A',
    'rss $F046B', 'rss-box $F046C', 'ruler $F046D',
    'run-fast $F046E', 'sale $F046F', 'satellite $F0470',
    'satellite-variant $F0471', 'scale $F0472', 'scale-bathroom $F0473',
    'school $F0474', 'screen-rotation $F0475', 'screwdriver $F0476',
    'script-outline $F0477', 'screen-rotation-lock $F0478', 'sd $F0479',
    'seal $F047A', 'seat-flat $F047B', 'seat-flat-angled $F047C',
    'seat-individual-suite $F047D', 'seat-legroom-extra $F047E', 'seat-legroom-normal $F047F',
    'seat-legroom-reduced $F0480', 'seat-recline-extra $F0481', 'seat-recline-normal $F0482',
    'security $F0483', 'security-network $F0484', 'select $F0485',
    'select-all $F0486', 'select-inverse $F0487', 'select-off $F0488',
    'selection $F0489', 'send $F048A', 'server $F048B',
    'server-minus $F048C', 'server-network $F048D', 'server-network-off $F048E',
    'server-off $F048F', 'server-plus $F0490', 'server-remove $F0491',
    'server-security $F0492', 'cog $F0493', 'cog-box $F0494',
    'shape-plus $F0495', 'share $F0496', 'share-variant $F0497',
    'shield $F0498', 'shield-outline $F0499', 'shopping $F049A',
    'shopping-music $F049B', 'shredder $F049C', 'shuffle $F049D',
    'shuffle-disabled $F049E', 'shuffle-variant $F049F', 'sigma $F04A0',
    'sign-caution $F04A1', 'signal $F04A2', 'silverware $F04A3',
    'silverware-fork $F04A4', 'silverware-spoon $F04A5', 'silverware-variant $F04A6',
    'sim $F04A7', 'sim-alert $F04A8', 'sim-off $F04A9',
    'sitemap $F04AA', 'skip-backward $F04AB', 'skip-forward $F04AC',
    'skip-next $F04AD', 'skip-previous $F04AE', 'skype $F04AF',
    'skype-business $F04B0', 'slack $F04B1', 'sleep $F04B2',
    'sleep-off $F04B3', 'smoking $F04B4', 'smoking-off $F04B5',
    'snapchat $F04B6', 'snowman $F04B7', 'soccer $F04B8',
    'sofa $F04B9', 'sort $F04BA', 'sort-alphabetical-variant $F04BB',
    'sort-ascending $F04BC', 'sort-descending $F04BD', 'sort-numeric-variant $F04BE',
    'sort-variant $F04BF', 'soundcloud $F04C0', 'source-fork $F04C1',
    'source-pull $F04C2', 'speaker $F04C3', 'speaker-off $F04C4',
    'speedometer $F04C5', 'spellcheck $F04C6', 'spotify $F04C7',
    'spotlight $F04C8', 'spotlight-beam $F04C9', 'book-remove-multiple-outline $F04CA',
    'account-switch-outline $F04CB', 'stack-overflow $F04CC', 'stairs $F04CD',
    'star $F04CE', 'star-circle $F04CF', 'star-half-full $F04D0',
    'star-off $F04D1', 'star-outline $F04D2', 'steam $F04D3',
    'steering $F04D4', 'step-backward $F04D5', 'step-backward-2 $F04D6',
    'step-forward $F04D7', 'step-forward-2 $F04D8', 'stethoscope $F04D9',
    'stocking $F04DA', 'stop $F04DB', 'store $F04DC',
    'store-24-hour $F04DD', 'stove $F04DE', 'subway-variant $F04DF',
    'sunglasses $F04E0', 'swap-horizontal $F04E1', 'swap-vertical $F04E2',
    'swim $F04E3', 'switch $F04E4', 'sword $F04E5',
    'sync $F04E6', 'sync-alert $F04E7', 'sync-off $F04E8',
    'tab $F04E9', 'tab-unselected $F04EA', 'table $F04EB',
    'table-column-plus-after $F04EC', 'table-column-plus-before $F04ED', 'table-column-remove $F04EE',
    'table-column-width $F04EF', 'table-edit $F04F0', 'table-large $F04F1',
    'table-row-height $F04F2', 'table-row-plus-after $F04F3', 'table-row-plus-before $F04F4',
    'table-row-remove $F04F5', 'tablet $F04F6', 'tablet-android $F04F7',
    'tangram $F04F8', 'tag $F04F9', 'tag-faces $F04FA',
    'tag-multiple $F04FB', 'tag-outline $F04FC', 'tag-text-outline $F04FD',
    'target $F04FE', 'taxi $F04FF', 'teamviewer $F0500',
    'skateboarding $F0501', 'television $F0502', 'television-guide $F0503',
    'temperature-celsius $F0504', 'temperature-fahrenheit $F0505', 'temperature-kelvin $F0506',
    'tennis-ball $F0507', 'tent $F0508', 'text-to-speech $F050A',
    'text-to-speech-off $F050B', 'texture $F050C', 'theater $F050D',
    'theme-light-dark $F050E', 'thermometer $F050F', 'thermometer-lines $F0510',
    'thumb-down $F0511', 'thumb-down-outline $F0512', 'thumb-up $F0513',
    'thumb-up-outline $F0514', 'thumbs-up-down $F0515', 'ticket $F0516',
    'ticket-account $F0517', 'ticket-confirmation $F0518', 'tie $F0519',
    'timelapse $F051A', 'timer-outline $F051B', 'timer-10 $F051C',
    'timer-3 $F051D', 'timer-off-outline $F051E', 'timer-sand $F051F',
    'timetable $F0520', 'toggle-switch $F0521', 'toggle-switch-off $F0522',
    'tooltip $F0523', 'tooltip-edit $F0524', 'tooltip-image $F0525',
    'tooltip-outline $F0526', 'tooltip-plus-outline $F0527', 'tooltip-text $F0528',
    'tooth-outline $F0529', 'cloud-refresh $F052A', 'traffic-light $F052B',
    'train $F052C', 'tram $F052D', 'transcribe $F052E',
    'transcribe-close $F052F', 'transfer-right $F0530', 'tree $F0531',
    'trello $F0532', 'trending-down $F0533', 'trending-neutral $F0534',
    'trending-up $F0535', 'triangle $F0536', 'triangle-outline $F0537',
    'trophy $F0538', 'trophy-award $F0539', 'trophy-outline $F053A',
    'trophy-variant $F053B', 'trophy-variant-outline $F053C', 'truck $F053D',
    'truck-delivery $F053E', 'tshirt-crew-outline $F053F', 'tshirt-v-outline $F0540',
    'file-refresh-outline $F0541', 'folder-refresh-outline $F0542', 'twitch $F0543',
    'twitter $F0544', 'order-numeric-ascending $F0545', 'order-numeric-descending $F0546',
    'repeat-variant $F0547', 'ubuntu $F0548', 'umbraco $F0549',
    'umbrella $F054A', 'umbrella-outline $F054B', 'undo $F054C',
    'undo-variant $F054D', 'unfold-less-horizontal $F054E', 'unfold-more-horizontal $F054F',
    'ungroup $F0550', 'web-remove $F0551', 'upload $F0552',
    'usb $F0553', 'vector-arrange-above $F0554', 'vector-arrange-below $F0555',
    'vector-circle $F0556', 'vector-circle-variant $F0557', 'vector-combine $F0558',
    'vector-curve $F0559', 'vector-difference $F055A', 'vector-difference-ab $F055B',
    'vector-difference-ba $F055C', 'vector-intersection $F055D', 'vector-line $F055E',
    'vector-point $F055F', 'vector-polygon $F0560', 'vector-polyline $F0561',
    'vector-selection $F0562', 'vector-triangle $F0563', 'vector-union $F0564',
    'shield-check $F0565', 'vibrate $F0566', 'video $F0567',
    'video-off $F0568', 'video-switch $F0569', 'view-agenda $F056A',
    'view-array $F056B', 'view-carousel $F056C', 'view-column $F056D',
    'view-dashboard $F056E', 'view-day $F056F', 'view-grid $F0570',
    'view-headline $F0571', 'view-list $F0572', 'view-module $F0573',
    'view-quilt $F0574', 'view-stream $F0575', 'view-week $F0576',
    'vimeo $F0577', 'buffet $F0578', 'hands-pray $F0579',
    'credit-card-wireless-off $F057A', 'credit-card-wireless-off-outline $F057B', 'vlc $F057C',
    'voicemail $F057D', 'volume-high $F057E', 'volume-low $F057F',
    'volume-medium $F0580', 'volume-off $F0581', 'vpn $F0582',
    'walk $F0583', 'wallet $F0584', 'wallet-giftcard $F0585',
    'wallet-membership $F0586', 'wallet-travel $F0587', 'wan $F0588',
    'watch $F0589', 'watch-export $F058A', 'watch-import $F058B',
    'water $F058C', 'water-off $F058D', 'water-percent $F058E',
    'water-pump $F058F', 'weather-cloudy $F0590', 'weather-fog $F0591',
    'weather-hail $F0592', 'weather-lightning $F0593', 'weather-night $F0594',
    'weather-partly-cloudy $F0595', 'weather-pouring $F0596', 'weather-rainy $F0597',
    'weather-snowy $F0598', 'weather-sunny $F0599', 'weather-sunset $F059A',
    'weather-sunset-down $F059B', 'weather-sunset-up $F059C', 'weather-windy $F059D',
    'weather-windy-variant $F059E', 'web $F059F', 'webcam $F05A0',
    'weight $F05A1', 'weight-kilogram $F05A2', 'whatsapp $F05A3',
    'wheelchair-accessibility $F05A4', 'white-balance-auto $F05A5', 'white-balance-incandescent $F05A6',
    'white-balance-iridescent $F05A7', 'white-balance-sunny $F05A8', 'wifi $F05A9',
    'wifi-off $F05AA', 'nintendo-wii $F05AB', 'wikipedia $F05AC',
    'window-close $F05AD', 'window-closed $F05AE', 'window-maximize $F05AF',
    'window-minimize $F05B0', 'window-open $F05B1', 'window-restore $F05B2',
    'microsoft-windows $F05B3', 'wordpress $F05B4', 'account-hard-hat $F05B5',
    'wrap $F05B6', 'wrench $F05B7', 'contacts-outline $F05B8',
    'microsoft-xbox $F05B9', 'microsoft-xbox-controller $F05BA', 'microsoft-xbox-controller-off $F05BB',
    'table-furniture $F05BC', 'sort-alphabetical-ascending $F05BD', 'firewire $F05BE',
    'sort-alphabetical-descending $F05BF', 'xml $F05C0', 'yeast $F05C1',
    'database-refresh $F05C2', 'youtube $F05C3', 'zip-box $F05C4',
    'surround-sound $F05C5', 'vector-rectangle $F05C6', 'playlist-check $F05C7',
    'format-line-style $F05C8', 'format-line-weight $F05C9', 'translate $F05CA',
    'account-voice $F05CB', 'opacity $F05CC', 'near-me $F05CD',
    'clock-alert-outline $F05CE', 'human-pregnant $F05CF', 'sticker-circle-outline $F05D0',
    'scale-balance $F05D1', 'card-account-details $F05D2', 'account-multiple-minus $F05D3',
    'airplane-landing $F05D4', 'airplane-takeoff $F05D5', 'alert-circle-outline $F05D6',
    'altimeter $F05D7', 'animation $F05D8', 'book-minus $F05D9',
    'book-open-page-variant $F05DA', 'book-plus $F05DB', 'boombox $F05DC',
    'bullseye $F05DD', 'comment-remove $F05DE', 'camera-off $F05DF',
    'check-circle $F05E0', 'check-circle-outline $F05E1', 'candle $F05E2',
    'chart-bubble $F05E3', 'credit-card-off-outline $F05E4', 'cup-off $F05E5',
    'copyright $F05E6', 'cursor-text $F05E7', 'delete-forever $F05E8',
    'delete-sweep $F05E9', 'dice-d20-outline $F05EA', 'dice-d4-outline $F05EB',
    'dice-d8-outline $F05EC', 'dice-d6-outline $F05ED', 'disc $F05EE',
    'email-open-outline $F05EF', 'email-variant $F05F0', 'ev-station $F05F1',
    'food-fork-drink $F05F2', 'food-off $F05F3', 'format-title $F05F4',
    'google-maps $F05F5', 'heart-pulse $F05F6', 'highway $F05F7',
    'home-map-marker $F05F8', 'incognito $F05F9', 'kettle $F05FA',
    'lock-plus $F05FB', 'logout-variant $F05FD', 'music-note-bluetooth $F05FE',
    'music-note-bluetooth-off $F05FF', 'page-first $F0600', 'page-last $F0601',
    'phone-classic $F0602', 'priority-high $F0603', 'priority-low $F0604',
    'qqchat $F0605', 'pool $F0606', 'rounded-corner $F0607',
    'rowing $F0608', 'saxophone $F0609', 'signal-variant $F060A',
    'stack-exchange $F060B', 'subdirectory-arrow-left $F060C', 'subdirectory-arrow-right $F060D',
    'form-textbox $F060E', 'violin $F060F', 'microsoft-visual-studio $F0610',
    'wechat $F0611', 'watermark $F0612', 'file-hidden $F0613',
    'application-outline $F0614', 'arrow-collapse $F0615', 'arrow-expand $F0616',
    'bowl-mix $F0617', 'bridge $F0618', 'application-edit-outline $F0619',
    'chip $F061A', 'content-save-settings $F061B', 'dialpad $F061C',
    'book-alphabet $F061D', 'format-horizontal-align-center $F061E', 'format-horizontal-align-left $F061F',
    'format-horizontal-align-right $F0620', 'format-vertical-align-bottom $F0621', 'format-vertical-align-center $F0622',
    'format-vertical-align-top $F0623', 'line-scan $F0624', 'help-circle-outline $F0625',
    'code-json $F0626', 'lambda $F0627', 'matrix $F0628',
    'meteor $F0629', 'close-circle-multiple $F062A', 'sigma-lower $F062B',
    'source-branch $F062C', 'source-merge $F062D', 'tune $F062E',
    'webhook $F062F', 'account-settings $F0630', 'account-details $F0631',
    'apple-keyboard-caps $F0632', 'apple-keyboard-command $F0633', 'apple-keyboard-control $F0634',
    'apple-keyboard-option $F0635', 'apple-keyboard-shift $F0636', 'box-shadow $F0637',
    'cards $F0638', 'cards-outline $F0639', 'cards-playing-outline $F063A',
    'checkbox-multiple-blank-circle $F063B', 'checkbox-multiple-blank-circle-outline $F063C', 'checkbox-multiple-marked-circle $F063D',
    'checkbox-multiple-marked-circle-outline $F063E', 'cloud-sync $F063F', 'collage $F0640',
    'directions-fork $F0641', 'eraser-variant $F0642', 'face-man $F0643',
    'face-man-profile $F0644', 'file-tree $F0645', 'format-annotation-plus $F0646',
    'gas-cylinder $F0647', 'grease-pencil $F0648', 'human-female $F0649',
    'human-greeting-variant $F064A', 'human-handsdown $F064B', 'human-handsup $F064C',
    'human-male $F064D', 'information-variant $F064E', 'lead-pencil $F064F',
    'map-marker-minus $F0650', 'map-marker-plus $F0651', 'marker $F0652',
    'message-plus $F0653', 'microscope $F0654', 'move-resize $F0655',
    'move-resize-variant $F0656', 'paw-off $F0657', 'phone-minus $F0658',
    'phone-plus $F0659', 'pot-steam $F065A', 'pot-mix $F065B',
    'serial-port $F065C', 'shape-circle-plus $F065D', 'shape-polygon-plus $F065E',
    'shape-rectangle-plus $F065F', 'shape-square-plus $F0660', 'skip-next-circle $F0661',
    'skip-next-circle-outline $F0662', 'skip-previous-circle $F0663', 'skip-previous-circle-outline $F0664',
    'spray $F0665', 'stop-circle $F0666', 'stop-circle-outline $F0667',
    'test-tube $F0668', 'text-shadow $F0669', 'tune-vertical $F066A',
    'cart-off $F066B', 'chart-gantt $F066C', 'chart-scatter-plot-hexbin $F066D',
    'chart-timeline $F066E', 'discord $F066F', 'file-restore $F0670',
    'language-c $F0671', 'language-cpp $F0672', 'language-xaml $F0673',
    'creation $F0674', 'application-cog $F0675', 'credit-card-plus-outline $F0676',
    'pot-mix-outline $F0677', 'bow-tie $F0678', 'calendar-range $F0679',
    'currency-usd-off $F067A', 'flash-red-eye $F067B', 'oar $F067C',
    'piano $F067D', 'weather-lightning-rainy $F067E', 'weather-snowy-rainy $F067F',
    'yin-yang $F0680', 'tower-beach $F0681', 'tower-fire $F0682',
    'delete-circle $F0683', 'dna $F0684', 'hamburger $F0685',
    'gondola $F0686', 'inbox $F0687', 'reorder-horizontal $F0688',
    'reorder-vertical $F0689', 'shield-home $F068A', 'tag-heart $F068B',
    'skull $F068C', 'solid $F068D', 'alarm-snooze $F068E',
    'baby-carriage $F068F', 'beaker-outline $F0690', 'bomb $F0691',
    'calendar-question $F0692', 'camera-burst $F0693', 'code-tags-check $F0694',
    'circle-multiple-outline $F0695', 'crop-rotate $F0696', 'developer-board $F0697',
    'piano-off $F0698', 'skate-off $F0699', 'message-star $F069A',
    'emoticon-dead-outline $F069B', 'emoticon-excited-outline $F069C', 'folder-star $F069D',
    'format-color-text $F069E', 'format-section $F069F', 'gradient-vertical $F06A0',
    'home-outline $F06A1', 'message-bulleted $F06A2', 'message-bulleted-off $F06A3',
    'nuke $F06A4', 'power-plug $F06A5', 'power-plug-off $F06A6',
    'publish $F06A7', 'credit-card-marker $F06A8', 'robot $F06A9',
    'format-rotate-90 $F06AA', 'scanner $F06AB', 'subway $F06AC',
    'timer-sand-empty $F06AD', 'transit-transfer $F06AE', 'unity $F06AF',
    'update $F06B0', 'watch-vibrate $F06B1', 'angular $F06B2',
    'dolby $F06B3', 'emby $F06B4', 'lamp $F06B5',
    'menu-down-outline $F06B6', 'menu-up-outline $F06B7', 'note-multiple $F06B8',
    'note-multiple-outline $F06B9', 'plex $F06BA', 'shield-airplane $F06BB',
    'account-edit $F06BC', 'alert-decagram $F06BD', 'all-inclusive $F06BE',
    'angularjs $F06BF', 'arrow-down-box $F06C0', 'arrow-left-box $F06C1',
    'arrow-right-box $F06C2', 'arrow-up-box $F06C3', 'asterisk $F06C4',
    'bomb-off $F06C5', 'bootstrap $F06C6', 'cards-variant $F06C7',
    'clipboard-flow $F06C8', 'close-outline $F06C9', 'coffee-outline $F06CA',
    'contacts $F06CB', 'delete-empty $F06CC', 'earth-box $F06CD',
    'earth-box-off $F06CE', 'email-alert $F06CF', 'eye-outline $F06D0',
    'eye-off-outline $F06D1', 'fast-forward-outline $F06D2', 'feather $F06D3',
    'find-replace $F06D4', 'flash-outline $F06D5', 'format-font $F06D6',
    'format-page-break $F06D7', 'format-pilcrow $F06D8', 'garage $F06D9',
    'garage-open $F06DA', 'card-account-details-star-outline $F06DB', 'google-keep $F06DC',
    'snowmobile $F06DD', 'heart-half-full $F06DE', 'heart-half $F06DF',
    'heart-half-outline $F06E0', 'hexagon-multiple $F06E1', 'hook $F06E2',
    'hook-off $F06E3', 'infinity $F06E4', 'language-swift $F06E5',
    'language-typescript $F06E6', 'laptop-off $F06E7', 'lightbulb-on $F06E8',
    'lightbulb-on-outline $F06E9', 'lock-pattern $F06EA', 'folder-zip $F06EB',
    'magnify-minus-outline $F06EC', 'magnify-plus-outline $F06ED', 'mailbox $F06EE',
    'medical-bag $F06EF', 'message-settings $F06F0', 'message-cog $F06F1',
    'minus-box-outline $F06F2', 'network $F06F3', 'download-network $F06F4',
    'help-network $F06F5', 'upload-network $F06F6', 'npm $F06F7',
    'nut $F06F8', 'octagram $F06F9', 'page-layout-body $F06FA',
    'page-layout-footer $F06FB', 'page-layout-header $F06FC', 'page-layout-sidebar-left $F06FD',
    'page-layout-sidebar-right $F06FE', 'pencil-circle $F06FF', 'pentagon-outline $F0700',
    'pentagon $F0701', 'pillar $F0702', 'pistol $F0703',
    'plus-box-outline $F0704', 'plus-outline $F0705', 'prescription $F0706',
    'printer-settings $F0707', 'react $F0708', 'restart $F0709',
    'rewind-outline $F070A', 'rhombus $F070B', 'rhombus-outline $F070C',
    'robot-vacuum $F070D', 'run $F070E', 'search-web $F070F',
    'shovel $F0710', 'shovel-off $F0711', 'signal-2g $F0712',
    'signal-3g $F0713', 'signal-4g $F0714', 'signal-hspa $F0715',
    'signal-hspa-plus $F0716', 'snowflake $F0717', 'source-commit $F0718',
    'source-commit-end $F0719', 'source-commit-end-local $F071A', 'source-commit-local $F071B',
    'source-commit-next-local $F071C', 'source-commit-start $F071D', 'source-commit-start-next-local $F071E',
    'speaker-wireless $F071F', 'stadium-variant $F0720', 'svg $F0721',
    'tag-plus $F0722', 'tag-remove $F0723', 'ticket-percent $F0724',
    'tilde $F0725', 'treasure-chest $F0726', 'truck-trailer $F0727',
    'view-parallel $F0728', 'view-sequential $F0729', 'washing-machine $F072A',
    'webpack $F072B', 'widgets $F072C', 'nintendo-wiiu $F072D',
    'arrow-down-bold $F072E', 'arrow-down-bold-box $F072F', 'arrow-down-bold-box-outline $F0730',
    'arrow-left-bold $F0731', 'arrow-left-bold-box $F0732', 'arrow-left-bold-box-outline $F0733',
    'arrow-right-bold $F0734', 'arrow-right-bold-box $F0735', 'arrow-right-bold-box-outline $F0736',
    'arrow-up-bold $F0737', 'arrow-up-bold-box $F0738', 'arrow-up-bold-box-outline $F0739',
    'cancel $F073A', 'file-account $F073B', 'gesture-double-tap $F073C',
    'gesture-swipe-down $F073D', 'gesture-swipe-left $F073E', 'gesture-swipe-right $F073F',
    'gesture-swipe-up $F0740', 'gesture-tap $F0741', 'gesture-two-double-tap $F0742',
    'gesture-two-tap $F0743', 'humble-bundle $F0744', 'kickstarter $F0745',
    'netflix $F0746', 'microsoft-onenote $F0747', 'wall-sconce-round $F0748',
    'folder-refresh $F0749', 'vector-radius $F074A', 'microsoft-xbox-controller-battery-alert $F074B',
    'microsoft-xbox-controller-battery-empty $F074C', 'microsoft-xbox-controller-battery-full $F074D', 'microsoft-xbox-controller-battery-low $F074E',
    'microsoft-xbox-controller-battery-medium $F074F', 'microsoft-xbox-controller-battery-unknown $F0750', 'clipboard-plus $F0751',
    'file-plus $F0752', 'format-align-bottom $F0753', 'format-align-middle $F0754',
    'format-align-top $F0755', 'format-list-checks $F0756', 'format-quote-open $F0757',
    'grid-large $F0758', 'heart-off $F0759', 'music $F075A',
    'music-off $F075B', 'tab-plus $F075C', 'volume-plus $F075D',
    'volume-minus $F075E', 'volume-mute $F075F', 'unfold-less-vertical $F0760',
    'unfold-more-vertical $F0761', 'taco $F0762', 'square-outline $F0763',
    'square $F0764', 'alert-octagram $F0767', 'atom $F0768',
    'ceiling-light $F0769', 'chart-bar-stacked $F076A', 'chart-line-stacked $F076B',
    'decagram $F076C', 'decagram-outline $F076D', 'dice-multiple $F076E',
    'dice-d10-outline $F076F', 'folder-open $F0770', 'guitar-acoustic $F0771',
    'loading $F0772', 'lock-reset $F0773', 'ninja $F0774',
    'octagram-outline $F0775', 'pencil-circle-outline $F0776', 'selection-off $F0777',
    'set-all $F0778', 'set-center $F0779', 'set-center-right $F077A',
    'set-left $F077B', 'set-left-center $F077C', 'set-left-right $F077D',
    'set-none $F077E', 'set-right $F077F', 'shield-half-full $F0780',
    'sign-direction $F0781', 'sign-text $F0782', 'signal-off $F0783',
    'square-root $F0784', 'sticker-emoji $F0785', 'summit $F0786',
    'sword-cross $F0787', 'truck-fast $F0788', 'web-check $F0789',
    'cast-off $F078A', 'help-box $F078B', 'timer-sand-full $F078C',
    'waves $F078D', 'alarm-bell $F078E', 'alarm-light $F078F',
    'video-switch-outline $F0790', 'check-decagram $F0791', 'arrow-collapse-down $F0792',
    'arrow-collapse-left $F0793', 'arrow-collapse-right $F0794', 'arrow-collapse-up $F0795',
    'arrow-expand-down $F0796', 'arrow-expand-left $F0797', 'arrow-expand-right $F0798',
    'arrow-expand-up $F0799', 'book-lock $F079A', 'book-lock-open $F079B',
    'bus-articulated-end $F079C', 'bus-articulated-front $F079D', 'bus-double-decker $F079E',
    'bus-school $F079F', 'bus-side $F07A0', 'camera-gopro $F07A1',
    'camera-metering-center $F07A2', 'camera-metering-matrix $F07A3', 'camera-metering-partial $F07A4',
    'camera-metering-spot $F07A5', 'cannabis $F07A6', 'car-convertible $F07A7',
    'car-estate $F07A8', 'car-hatchback $F07A9', 'car-pickup $F07AA',
    'car-side $F07AB', 'car-sports $F07AC', 'caravan $F07AD',
    'cctv $F07AE', 'chart-donut $F07AF', 'chart-donut-variant $F07B0',
    'chart-line-variant $F07B1', 'chili-hot $F07B2', 'chili-medium $F07B3',
    'chili-mild $F07B4', 'cloud-braces $F07B5', 'cloud-tags $F07B6',
    'console-line $F07B7', 'corn $F07B8', 'folder-zip-outline $F07B9',
    'currency-cny $F07BA', 'currency-eth $F07BB', 'currency-krw $F07BD',
    'currency-sign $F07BE', 'currency-twd $F07BF', 'desktop-classic $F07C0',
    'dip-switch $F07C1', 'donkey $F07C2', 'dots-horizontal-circle $F07C3',
    'dots-vertical-circle $F07C4', 'ear-hearing $F07C5', 'elephant $F07C6',
    'storefront $F07C7', 'food-croissant $F07C8', 'forklift $F07C9',
    'fuel $F07CA', 'gesture $F07CB', 'google-analytics $F07CC',
    'google-assistant $F07CD', 'headphones-off $F07CE', 'high-definition $F07CF',
    'home-assistant $F07D0', 'home-automation $F07D1', 'home-circle $F07D2',
    'language-go $F07D3', 'language-r $F07D4', 'lava-lamp $F07D5',
    'led-strip $F07D6', 'locker $F07D7', 'locker-multiple $F07D8',
    'map-marker-outline $F07D9', 'metronome $F07DA', 'metronome-tick $F07DB',
    'micro-sd $F07DC', 'facebook-gaming $F07DD', 'movie-roll $F07DE',
    'mushroom $F07DF', 'mushroom-outline $F07E0', 'nintendo-switch $F07E1',
    'null $F07E2', 'passport $F07E3', 'molecule-co2 $F07E4',
    'pipe $F07E5', 'pipe-disconnected $F07E6', 'power-socket-eu $F07E7',
    'power-socket-uk $F07E8', 'power-socket-us $F07E9', 'rice $F07EA',
    'ring $F07EB', 'sass $F07EC', 'send-lock $F07ED',
    'soy-sauce $F07EE', 'standard-definition $F07EF', 'surround-sound-2-0 $F07F0',
    'surround-sound-3-1 $F07F1', 'surround-sound-5-1 $F07F2', 'surround-sound-7-1 $F07F3',
    'television-classic $F07F4', 'form-textbox-password $F07F5', 'thought-bubble $F07F6',
    'thought-bubble-outline $F07F7', 'trackpad $F07F8', 'ultra-high-definition $F07F9',
    'van-passenger $F07FA', 'van-utility $F07FB', 'vanish $F07FC',
    'video-3d $F07FD', 'wall $F07FE', 'xmpp $F07FF',
    'account-multiple-plus-outline $F0800', 'account-plus-outline $F0801', 'credit-card-wireless $F0802',
    'account-music $F0803', 'atlassian $F0804', 'microsoft-azure $F0805',
    'basketball $F0806', 'battery-charging-wireless $F0807', 'battery-charging-wireless-10 $F0808',
    'battery-charging-wireless-20 $F0809', 'battery-charging-wireless-30 $F080A', 'battery-charging-wireless-40 $F080B',
    'battery-charging-wireless-50 $F080C', 'battery-charging-wireless-60 $F080D', 'battery-charging-wireless-70 $F080E',
    'battery-charging-wireless-80 $F080F', 'battery-charging-wireless-90 $F0810', 'battery-charging-wireless-alert $F0811',
    'battery-charging-wireless-outline $F0812', 'bitcoin $F0813', 'briefcase-outline $F0814',
    'cellphone-wireless $F0815', 'clover $F0816', 'comment-question $F0817',
    'content-save-outline $F0818', 'delete-restore $F0819', 'door $F081A',
    'door-closed $F081B', 'door-open $F081C', 'fan-off $F081D',
    'file-percent $F081E', 'finance $F081F', 'lightning-bolt-circle $F0820',
    'floor-plan $F0821', 'forum-outline $F0822', 'golf $F0823',
    'google-home $F0824', 'guy-fawkes-mask $F0825', 'home-account $F0826',
    'home-heart $F0827', 'hot-tub $F0828', 'hulu $F0829',
    'ice-cream $F082A', 'image-off $F082B', 'karate $F082C',
    'ladybug $F082D', 'notebook $F082E', 'phone-return $F082F',
    'poker-chip $F0830', 'shape $F0831', 'shape-outline $F0832',
    'ship-wheel $F0833', 'soccer-field $F0834', 'table-column $F0835',
    'table-of-contents $F0836', 'table-row $F0837', 'table-settings $F0838',
    'television-box $F0839', 'television-classic-off $F083A', 'television-off $F083B',
    'tow-truck $F083C', 'upload-multiple $F083D', 'video-4k-box $F083E',
    'video-input-antenna $F083F', 'video-input-component $F0840', 'video-input-hdmi $F0841',
    'video-input-svideo $F0842', 'view-dashboard-variant $F0843', 'vuejs $F0844',
    'xamarin $F0845', 'human-male-board-poll $F0846', 'youtube-studio $F0847',
    'youtube-gaming $F0848', 'account-group $F0849', 'camera-switch-outline $F084A',
    'airport $F084B', 'arrow-collapse-horizontal $F084C', 'arrow-collapse-vertical $F084D',
    'arrow-expand-horizontal $F084E', 'arrow-expand-vertical $F084F', 'augmented-reality $F0850',
    'badminton $F0851', 'baseball $F0852', 'baseball-bat $F0853',
    'bottle-wine $F0854', 'check-outline $F0855', 'checkbox-intermediate $F0856',
    'chess-king $F0857', 'chess-knight $F0858', 'chess-pawn $F0859',
    'chess-queen $F085A', 'chess-rook $F085B', 'chess-bishop $F085C',
    'clipboard-pulse $F085D', 'clipboard-pulse-outline $F085E', 'comment-multiple $F085F',
    'comment-text-multiple $F0860', 'comment-text-multiple-outline $F0861', 'crane $F0862',
    'curling $F0863', 'currency-bdt $F0864', 'currency-kzt $F0865',
    'database-search $F0866', 'dice-d12-outline $F0867', 'docker $F0868',
    'doorbell-video $F0869', 'ethereum $F086A', 'eye-plus $F086B',
    'eye-plus-outline $F086C', 'eye-settings $F086D', 'eye-settings-outline $F086E',
    'file-question $F086F', 'folder-network $F0870', 'function-variant $F0871',
    'garage-alert $F0872', 'gauge-empty $F0873', 'gauge-full $F0874',
    'gauge-low $F0875', 'glass-wine $F0876', 'graphql $F0877',
    'high-definition-box $F0878', 'hockey-puck $F0879', 'hockey-sticks $F087A',
    'home-alert $F087B', 'image-plus $F087C', 'jquery $F087D',
    'lifebuoy $F087E', 'mixed-reality $F087F', 'nativescript $F0880',
    'onepassword $F0881', 'patreon $F0882', 'close-circle-multiple-outline $F0883',
    'peace $F0884', 'phone-rotate-landscape $F0885', 'phone-rotate-portrait $F0886',
    'pier $F0887', 'pier-crane $F0888', 'pipe-leak $F0889',
    'piston $F088A', 'play-network $F088B', 'reminder $F088C',
    'room-service $F088D', 'salesforce $F088E', 'shield-account $F088F',
    'human-male-board $F0890', 'thermostat-box $F0891', 'tractor $F0892',
    'vector-ellipse $F0893', 'virtual-reality $F0894', 'watch-export-variant $F0895',
    'watch-import-variant $F0896', 'watch-variant $F0897', 'weather-hurricane $F0898',
    'account-heart $F0899', 'alien $F089A', 'anvil $F089B',
    'battery-charging-10 $F089C', 'battery-charging-50 $F089D', 'battery-charging-70 $F089E',
    'battery-charging-outline $F089F', 'bed-empty $F08A0', 'border-all-variant $F08A1',
    'border-bottom-variant $F08A2', 'border-left-variant $F08A3', 'border-none-variant $F08A4',
    'border-right-variant $F08A5', 'border-top-variant $F08A6', 'calendar-edit $F08A7',
    'clipboard-check-outline $F08A8', 'console-network $F08A9', 'file-compare $F08AA',
    'fire-truck $F08AB', 'folder-key $F08AC', 'folder-key-network $F08AD',
    'expansion-card $F08AE', 'kayaking $F08AF', 'inbox-multiple $F08B0',
    'language-lua $F08B1', 'lock-smart $F08B2', 'microphone-minus $F08B3',
    'microphone-plus $F08B4', 'palette-swatch $F08B5', 'periodic-table $F08B6',
    'pickaxe $F08B7', 'qrcode-edit $F08B8', 'remote-desktop $F08B9',
    'sausage $F08BA', 'cog-outline $F08BB', 'signal-cellular-1 $F08BC',
    'signal-cellular-2 $F08BD', 'signal-cellular-3 $F08BE', 'signal-cellular-outline $F08BF',
    'ssh $F08C0', 'swap-horizontal-variant $F08C1', 'swap-vertical-variant $F08C2',
    'tooth $F08C3', 'train-variant $F08C4', 'account-multiple-check $F08C5',
    'application $F08C6', 'arch $F08C7', 'axe $F08C8',
    'bullseye-arrow $F08C9', 'bus-clock $F08CA', 'camera-account $F08CB',
    'camera-image $F08CC', 'car-limousine $F08CD', 'cards-club $F08CE',
    'cards-diamond $F08CF', 'cards-spade $F08D1', 'cellphone-text $F08D2',
    'cellphone-message $F08D3', 'chart-multiline $F08D4', 'circle-edit-outline $F08D5',
    'cogs $F08D6', 'credit-card-settings-outline $F08D7', 'death-star $F08D8',
    'death-star-variant $F08D9', 'debian $F08DA', 'fedora $F08DB',
    'file-undo $F08DC', 'floor-lamp $F08DD', 'folder-edit $F08DE',
    'format-columns $F08DF', 'freebsd $F08E0', 'gate-and $F08E1',
    'gate-nand $F08E2', 'gate-nor $F08E3', 'gate-not $F08E4',
    'gate-or $F08E5', 'gate-xnor $F08E6', 'gate-xor $F08E7',
    'gentoo $F08E8', 'globe-model $F08E9', 'hammer $F08EA',
    'home-lock $F08EB', 'home-lock-open $F08EC', 'linux-mint $F08ED',
    'lock-alert $F08EE', 'lock-question $F08EF', 'map-marker-distance $F08F0',
    'midi $F08F1', 'midi-port $F08F2', 'nas $F08F3',
    'network-strength-1 $F08F4', 'network-strength-1-alert $F08F5', 'network-strength-2 $F08F6',
    'network-strength-2-alert $F08F7', 'network-strength-3 $F08F8', 'network-strength-3-alert $F08F9',
    'network-strength-4 $F08FA', 'network-strength-4-alert $F08FB', 'network-strength-off $F08FC',
    'network-strength-off-outline $F08FD', 'network-strength-outline $F08FE', 'play-speed $F08FF',
    'playlist-edit $F0900', 'power-cycle $F0901', 'power-off $F0902',
    'power-on $F0903', 'power-sleep $F0904', 'power-socket-au $F0905',
    'power-standby $F0906', 'rabbit $F0907', 'robot-vacuum-variant $F0908',
    'satellite-uplink $F0909', 'scanner-off $F090A', 'book-minus-multiple-outline $F090B',
    'square-edit-outline $F090C', 'sort-numeric-ascending-variant $F090D', 'steering-off $F090E',
    'table-search $F090F', 'tag-minus $F0910', 'test-tube-empty $F0911',
    'test-tube-off $F0912', 'ticket-outline $F0913', 'track-light $F0914',
    'transition $F0915', 'transition-masked $F0916', 'tumble-dryer $F0917',
    'file-refresh $F0918', 'video-account $F0919', 'video-image $F091A',
    'video-stabilization $F091B', 'wall-sconce $F091C', 'wall-sconce-flat $F091D',
    'wall-sconce-round-variant $F091E', 'wifi-strength-1 $F091F', 'wifi-strength-1-alert $F0920',
    'wifi-strength-1-lock $F0921', 'wifi-strength-2 $F0922', 'wifi-strength-2-alert $F0923',
    'wifi-strength-2-lock $F0924', 'wifi-strength-3 $F0925', 'wifi-strength-3-alert $F0926',
    'wifi-strength-3-lock $F0927', 'wifi-strength-4 $F0928', 'wifi-strength-4-alert $F0929',
    'wifi-strength-4-lock $F092A', 'wifi-strength-alert-outline $F092B', 'wifi-strength-lock-outline $F092C',
    'wifi-strength-off $F092D', 'wifi-strength-off-outline $F092E', 'wifi-strength-outline $F092F',
    'pin-off-outline $F0930', 'pin-outline $F0931', 'share-outline $F0932',
    'trackpad-lock $F0933', 'account-box-multiple $F0934', 'account-search-outline $F0935',
    'account-filter $F0936', 'angle-acute $F0937', 'angle-obtuse $F0938',
    'angle-right $F0939', 'animation-play $F093A', 'arrow-split-horizontal $F093B',
    'arrow-split-vertical $F093C', 'audio-video $F093D', 'battery-10-bluetooth $F093E',
    'battery-20-bluetooth $F093F', 'battery-30-bluetooth $F0940', 'battery-40-bluetooth $F0941',
    'battery-50-bluetooth $F0942', 'battery-60-bluetooth $F0943', 'battery-70-bluetooth $F0944',
    'battery-80-bluetooth $F0945', 'battery-90-bluetooth $F0946', 'battery-alert-bluetooth $F0947',
    'battery-bluetooth $F0948', 'battery-bluetooth-variant $F0949', 'battery-unknown-bluetooth $F094A',
    'dharmachakra $F094B', 'calendar-search $F094C', 'cellphone-remove $F094D',
    'cellphone-key $F094E', 'cellphone-lock $F094F', 'cellphone-off $F0950',
    'cellphone-cog $F0951', 'cellphone-sound $F0952', 'cross $F0953',
    'clock $F0954', 'clock-alert $F0955', 'cloud-search $F0956',
    'cloud-search-outline $F0957', 'cordova $F0958', 'cryengine $F0959',
    'cupcake $F095A', 'sine-wave $F095B', 'current-dc $F095C',
    'database-import $F095D', 'database-export $F095E', 'desk-lamp $F095F',
    'disc-player $F0960', 'email-search $F0961', 'email-search-outline $F0962',
    'exponent $F0963', 'exponent-box $F0964', 'file-download $F0965',
    'file-download-outline $F0966', 'firebase $F0967', 'folder-search $F0968',
    'folder-search-outline $F0969', 'format-list-checkbox $F096A', 'fountain $F096B',
    'google-fit $F096C', 'greater-than $F096D', 'greater-than-or-equal $F096E',
    'hard-hat $F096F', 'headphones-bluetooth $F0970', 'heart-circle $F0971',
    'heart-circle-outline $F0972', 'om $F0973', 'home-minus $F0974',
    'home-plus $F0975', 'image-outline $F0976', 'image-search $F0977',
    'image-search-outline $F0978', 'star-crescent $F0979', 'star-david $F097A',
    'keyboard-outline $F097B', 'less-than $F097C', 'less-than-or-equal $F097D',
    'light-switch $F097E', 'lock-clock $F097F', 'magnify-close $F0980',
    'map-minus $F0981', 'map-outline $F0982', 'map-plus $F0983',
    'map-search $F0984', 'map-search-outline $F0985', 'material-design $F0986',
    'medal $F0987', 'microsoft-dynamics-365 $F0988', 'monitor-cellphone $F0989',
    'monitor-cellphone-star $F098A', 'mouse-bluetooth $F098B', 'muffin $F098C',
    'not-equal $F098D', 'not-equal-variant $F098E', 'order-bool-ascending-variant $F098F',
    'order-bool-descending-variant $F0990', 'office-building $F0991', 'plus-minus $F0992',
    'plus-minus-box $F0993', 'podcast $F0994', 'progress-check $F0995',
    'progress-clock $F0996', 'progress-download $F0997', 'progress-upload $F0998',
    'qi $F0999', 'record-player $F099A', 'restore $F099B',
    'shield-off-outline $F099C', 'shield-lock $F099D', 'shield-off $F099E',
    'set-top-box $F099F', 'shower $F09A0', 'shower-head $F09A1',
    'speaker-bluetooth $F09A2', 'square-root-box $F09A3', 'star-circle-outline $F09A4',
    'star-face $F09A5', 'table-merge-cells $F09A6', 'tablet-cellphone $F09A7',
    'text $F09A8', 'text-short $F09A9', 'text-long $F09AA',
    'toilet $F09AB', 'toolbox $F09AC', 'toolbox-outline $F09AD',
    'tournament $F09AE', 'two-factor-authentication $F09AF', 'umbrella-closed $F09B0',
    'unreal $F09B1', 'video-minus $F09B2', 'video-plus $F09B3',
    'volleyball $F09B4', 'weight-pound $F09B5', 'whistle $F09B6',
    'arrow-bottom-left-bold-outline $F09B7', 'arrow-bottom-left-thick $F09B8', 'arrow-bottom-right-bold-outline $F09B9',
    'arrow-bottom-right-thick $F09BA', 'arrow-decision $F09BB', 'arrow-decision-auto $F09BC',
    'arrow-decision-auto-outline $F09BD', 'arrow-decision-outline $F09BE', 'arrow-down-bold-outline $F09BF',
    'arrow-left-bold-outline $F09C0', 'arrow-left-right-bold-outline $F09C1', 'arrow-right-bold-outline $F09C2',
    'arrow-top-left-bold-outline $F09C3', 'arrow-top-left-thick $F09C4', 'arrow-top-right-bold-outline $F09C5',
    'arrow-top-right-thick $F09C6', 'arrow-up-bold-outline $F09C7', 'arrow-up-down-bold-outline $F09C8',
    'ballot $F09C9', 'ballot-outline $F09CA', 'betamax $F09CB',
    'bookmark-minus $F09CC', 'bookmark-minus-outline $F09CD', 'bookmark-off $F09CE',
    'bookmark-off-outline $F09CF', 'braille $F09D0', 'brain $F09D1',
    'calendar-heart $F09D2', 'calendar-star $F09D3', 'cassette $F09D4',
    'cellphone-arrow-down $F09D5', 'chevron-down-box $F09D6', 'chevron-down-box-outline $F09D7',
    'chevron-left-box $F09D8', 'chevron-left-box-outline $F09D9', 'chevron-right-box $F09DA',
    'chevron-right-box-outline $F09DB', 'chevron-up-box $F09DC', 'chevron-up-box-outline $F09DD',
    'circle-medium $F09DE', 'circle-small $F09DF', 'cloud-alert $F09E0',
    'comment-arrow-left $F09E1', 'comment-arrow-left-outline $F09E2', 'comment-arrow-right $F09E3',
    'comment-arrow-right-outline $F09E4', 'comment-plus $F09E5', 'currency-php $F09E6',
    'delete-outline $F09E7', 'desktop-mac-dashboard $F09E8', 'download-multiple $F09E9',
    'eight-track $F09EA', 'email-plus $F09EB', 'email-plus-outline $F09EC',
    'text-box-outline $F09ED', 'file-document-outline $F09EE', 'floppy-variant $F09EF',
    'flower-outline $F09F0', 'flower-tulip $F09F1', 'flower-tulip-outline $F09F2',
    'format-font-size-decrease $F09F3', 'format-font-size-increase $F09F4', 'ghost-off $F09F5',
    'google-lens $F09F6', 'google-spreadsheet $F09F7', 'image-move $F09F8',
    'keyboard-settings $F09F9', 'keyboard-settings-outline $F09FA', 'knife $F09FB',
    'knife-military $F09FC', 'layers-off-outline $F09FD', 'layers-outline $F09FE',
    'lighthouse $F09FF', 'lighthouse-on $F0A00', 'map-legend $F0A01',
    'menu-left-outline $F0A02', 'menu-right-outline $F0A03', 'message-alert-outline $F0A04',
    'mini-sd $F0A05', 'minidisc $F0A06', 'monitor-dashboard $F0A07',
    'pirate $F0A08', 'pokemon-go $F0A09', 'powershell $F0A0A',
    'printer-wireless $F0A0B', 'quality-low $F0A0C', 'quality-medium $F0A0D',
    'reflect-horizontal $F0A0E', 'reflect-vertical $F0A0F', 'rhombus-medium $F0A10',
    'rhombus-split $F0A11', 'shield-account-outline $F0A12', 'square-medium $F0A13',
    'square-medium-outline $F0A14', 'square-small $F0A15', 'subtitles $F0A16',
    'subtitles-outline $F0A17', 'table-border $F0A18', 'toggle-switch-off-outline $F0A19',
    'toggle-switch-outline $F0A1A', 'vhs $F0A1B', 'video-vintage $F0A1C',
    'view-dashboard-outline $F0A1D', 'microsoft-visual-studio-code $F0A1E', 'vote $F0A1F',
    'vote-outline $F0A20', 'microsoft-windows-classic $F0A21', 'microsoft-xbox-controller-battery-charging $F0A22',
    'zip-disk $F0A23', 'aspect-ratio $F0A24', 'babel $F0A25',
    'balloon $F0A26', 'bank-transfer $F0A27', 'bank-transfer-in $F0A28',
    'bank-transfer-out $F0A29', 'briefcase-minus $F0A2A', 'briefcase-plus $F0A2B',
    'briefcase-remove $F0A2C', 'briefcase-search $F0A2D', 'bug-check $F0A2E',
    'bug-check-outline $F0A2F', 'bug-outline $F0A30', 'calendar-alert $F0A31',
    'calendar-multiselect $F0A32', 'calendar-week $F0A33', 'calendar-week-begin $F0A34',
    'cellphone-screenshot $F0A35', 'city-variant $F0A36', 'city-variant-outline $F0A37',
    'clipboard-text-outline $F0A38', 'cloud-question $F0A39', 'comment-eye $F0A3A',
    'comment-eye-outline $F0A3B', 'comment-search $F0A3C', 'comment-search-outline $F0A3D',
    'contain $F0A3E', 'contain-end $F0A3F', 'contain-start $F0A40',
    'dlna $F0A41', 'doctor $F0A42', 'dog $F0A43',
    'dog-side $F0A44', 'ear-hearing-off $F0A45', 'engine-off $F0A46',
    'engine-off-outline $F0A47', 'exit-run $F0A48', 'feature-search $F0A49',
    'feature-search-outline $F0A4A', 'file-alert $F0A4B', 'file-alert-outline $F0A4C',
    'file-upload $F0A4D', 'file-upload-outline $F0A4E', 'hand-front-right $F0A4F',
    'hand-okay $F0A50', 'hand-peace $F0A51', 'hand-peace-variant $F0A52',
    'hand-pointing-down $F0A53', 'hand-pointing-left $F0A54', 'hand-pointing-up $F0A55',
    'heart-multiple $F0A56', 'heart-multiple-outline $F0A57', 'horseshoe $F0A58',
    'human-female-boy $F0A59', 'human-female-female $F0A5A', 'human-female-girl $F0A5B',
    'human-male-boy $F0A5C', 'human-male-girl $F0A5D', 'human-male-male $F0A5E',
    'ip $F0A5F', 'ip-network $F0A60', 'litecoin $F0A61',
    'magnify-minus-cursor $F0A62', 'magnify-plus-cursor $F0A63', 'menu-swap $F0A64',
    'menu-swap-outline $F0A65', 'puzzle-outline $F0A66', 'registered-trademark $F0A67',
    'resize $F0A68', 'router-wireless-settings $F0A69', 'safe $F0A6A',
    'scissors-cutting $F0A6B', 'select-drag $F0A6C', 'selection-drag $F0A6D',
    'settings-helper $F0A6E', 'signal-5g $F0A6F', 'silverware-fork-knife $F0A70',
    'smog $F0A71', 'solar-power $F0A72', 'star-box $F0A73',
    'star-box-outline $F0A74', 'table-plus $F0A75', 'table-remove $F0A76',
    'target-variant $F0A77', 'trademark $F0A78', 'trash-can $F0A79',
    'trash-can-outline $F0A7A', 'tshirt-crew $F0A7B', 'tshirt-v $F0A7C',
    'zodiac-aquarius $F0A7D', 'zodiac-aries $F0A7E', 'zodiac-cancer $F0A7F',
    'zodiac-capricorn $F0A80', 'zodiac-gemini $F0A81', 'zodiac-leo $F0A82',
    'zodiac-libra $F0A83', 'zodiac-pisces $F0A84', 'zodiac-sagittarius $F0A85',
    'zodiac-scorpio $F0A86', 'zodiac-taurus $F0A87', 'zodiac-virgo $F0A88',
    'account-child $F0A89', 'account-child-circle $F0A8A', 'account-supervisor $F0A8B',
    'account-supervisor-circle $F0A8C', 'ampersand $F0A8D', 'web-off $F0A8E',
    'animation-outline $F0A8F', 'animation-play-outline $F0A90', 'bell-off-outline $F0A91',
    'bell-plus-outline $F0A92', 'bell-sleep-outline $F0A93', 'book-minus-multiple $F0A94',
    'book-plus-multiple $F0A95', 'book-remove-multiple $F0A96', 'book-remove $F0A97',
    'briefcase-edit $F0A98', 'bus-alert $F0A99', 'calculator-variant $F0A9A',
    'caps-lock $F0A9B', 'cash-refund $F0A9C', 'checkbook $F0A9D',
    'circle-slice-1 $F0A9E', 'circle-slice-2 $F0A9F', 'circle-slice-3 $F0AA0',
    'circle-slice-4 $F0AA1', 'circle-slice-5 $F0AA2', 'circle-slice-6 $F0AA3',
    'circle-slice-7 $F0AA4', 'circle-slice-8 $F0AA5', 'collapse-all $F0AA6',
    'collapse-all-outline $F0AA7', 'credit-card-refund-outline $F0AA8', 'database-check $F0AA9',
    'database-lock $F0AAA', 'desktop-tower-monitor $F0AAB', 'dishwasher $F0AAC',
    'dog-service $F0AAD', 'dot-net $F0AAE', 'egg $F0AAF',
    'egg-easter $F0AB0', 'email-check $F0AB1', 'email-check-outline $F0AB2',
    'et $F0AB3', 'expand-all $F0AB4', 'expand-all-outline $F0AB5',
    'file-cabinet $F0AB6', 'text-box-multiple $F0AB7', 'text-box-multiple-outline $F0AB8',
    'file-move $F0AB9', 'folder-clock $F0ABA', 'folder-clock-outline $F0ABB',
    'format-annotation-minus $F0ABC', 'gesture-pinch $F0ABD', 'gesture-spread $F0ABE',
    'gesture-swipe-horizontal $F0ABF', 'gesture-swipe-vertical $F0AC0', 'hail $F0AC1',
    'helicopter $F0AC2', 'hexagon-slice-1 $F0AC3', 'hexagon-slice-2 $F0AC4',
    'hexagon-slice-3 $F0AC5', 'hexagon-slice-4 $F0AC6', 'hexagon-slice-5 $F0AC7',
    'hexagon-slice-6 $F0AC8', 'hexagram $F0AC9', 'hexagram-outline $F0ACA',
    'label-off $F0ACB', 'label-off-outline $F0ACC', 'label-variant $F0ACD',
    'label-variant-outline $F0ACE', 'language-ruby-on-rails $F0ACF', 'laravel $F0AD0',
    'mastodon $F0AD1', 'sort-numeric-descending-variant $F0AD2', 'minus-circle-multiple-outline $F0AD3',
    'music-circle-outline $F0AD4', 'pinwheel $F0AD5', 'pinwheel-outline $F0AD6',
    'radiator-disabled $F0AD7', 'radiator-off $F0AD8', 'select-compare $F0AD9',
    'shield-plus $F0ADA', 'shield-plus-outline $F0ADB', 'shield-remove $F0ADC',
    'shield-remove-outline $F0ADD', 'book-plus-multiple-outline $F0ADE', 'sina-weibo $F0ADF',
    'spray-bottle $F0AE0', 'squeegee $F0AE1', 'star-four-points $F0AE2',
    'star-four-points-outline $F0AE3', 'star-three-points $F0AE4', 'star-three-points-outline $F0AE5',
    'symfony $F0AE6', 'variable $F0AE7', 'vector-bezier $F0AE8',
    'wiper $F0AE9', 'z-wave $F0AEA', 'zend $F0AEB',
    'account-minus-outline $F0AEC', 'account-remove-outline $F0AED', 'alpha-a $F0AEE',
    'alpha-b $F0AEF', 'alpha-c $F0AF0', 'alpha-d $F0AF1',
    'alpha-e $F0AF2', 'alpha-f $F0AF3', 'alpha-g $F0AF4',
    'alpha-h $F0AF5', 'alpha-i $F0AF6', 'alpha-j $F0AF7',
    'alpha-k $F0AF8', 'alpha-l $F0AF9', 'alpha-m $F0AFA',
    'alpha-n $F0AFB', 'alpha-o $F0AFC', 'alpha-p $F0AFD',
    'alpha-q $F0AFE', 'alpha-r $F0AFF', 'alpha-s $F0B00',
    'alpha-t $F0B01', 'alpha-u $F0B02', 'alpha-v $F0B03',
    'alpha-w $F0B04', 'alpha-x $F0B05', 'alpha-y $F0B06',
    'alpha-z $F0B07', 'alpha-a-box $F0B08', 'alpha-b-box $F0B09',
    'alpha-c-box $F0B0A', 'alpha-d-box $F0B0B', 'alpha-e-box $F0B0C',
    'alpha-f-box $F0B0D', 'alpha-g-box $F0B0E', 'alpha-h-box $F0B0F',
    'alpha-i-box $F0B10', 'alpha-j-box $F0B11', 'alpha-k-box $F0B12',
    'alpha-l-box $F0B13', 'alpha-m-box $F0B14', 'alpha-n-box $F0B15',
    'alpha-o-box $F0B16', 'alpha-p-box $F0B17', 'alpha-q-box $F0B18',
    'alpha-r-box $F0B19', 'alpha-s-box $F0B1A', 'alpha-t-box $F0B1B',
    'alpha-u-box $F0B1C', 'alpha-v-box $F0B1D', 'alpha-w-box $F0B1E',
    'alpha-x-box $F0B1F', 'alpha-y-box $F0B20', 'alpha-z-box $F0B21',
    'bulldozer $F0B22', 'bullhorn-outline $F0B23', 'calendar-export $F0B24',
    'calendar-import $F0B25', 'chevron-down-circle $F0B26', 'chevron-down-circle-outline $F0B27',
    'chevron-left-circle $F0B28', 'chevron-left-circle-outline $F0B29', 'chevron-right-circle $F0B2A',
    'chevron-right-circle-outline $F0B2B', 'chevron-up-circle $F0B2C', 'chevron-up-circle-outline $F0B2D',
    'content-save-settings-outline $F0B2E', 'crystal-ball $F0B2F', 'ember $F0B30',
    'facebook-workplace $F0B31', 'file-replace $F0B32', 'file-replace-outline $F0B33',
    'format-letter-case $F0B34', 'format-letter-case-lower $F0B35', 'format-letter-case-upper $F0B36',
    'language-java $F0B37', 'circle-multiple $F0B38', 'numeric-1 $F0B3A',
    'numeric-2 $F0B3B', 'numeric-3 $F0B3C', 'numeric-4 $F0B3D',
    'numeric-5 $F0B3E', 'numeric-6 $F0B3F', 'numeric-7 $F0B40',
    'numeric-8 $F0B41', 'numeric-9 $F0B42', 'origin $F0B43',
    'resistor $F0B44', 'resistor-nodes $F0B45', 'robot-industrial $F0B46',
    'shoe-formal $F0B47', 'shoe-heel $F0B48', 'silo $F0B49',
    'box-cutter-off $F0B4A', 'tab-minus $F0B4B', 'tab-remove $F0B4C',
    'tape-measure $F0B4D', 'telescope $F0B4E', 'yahoo $F0B4F',
    'account-alert-outline $F0B50', 'account-arrow-left $F0B51', 'account-arrow-left-outline $F0B52',
    'account-arrow-right $F0B53', 'account-arrow-right-outline $F0B54', 'account-circle-outline $F0B55',
    'account-clock $F0B56', 'account-clock-outline $F0B57', 'account-group-outline $F0B58',
    'account-question $F0B59', 'account-question-outline $F0B5A', 'artstation $F0B5B',
    'backspace-outline $F0B5C', 'barley-off $F0B5D', 'barn $F0B5E',
    'bat $F0B5F', 'application-settings $F0B60', 'billiards $F0B61',
    'billiards-rack $F0B62', 'book-open-outline $F0B63', 'book-outline $F0B64',
    'boxing-glove $F0B65', 'calendar-blank-outline $F0B66', 'calendar-outline $F0B67',
    'calendar-range-outline $F0B68', 'camera-control $F0B69', 'camera-enhance-outline $F0B6A',
    'car-door $F0B6B', 'car-electric $F0B6C', 'car-key $F0B6D',
    'car-multiple $F0B6E', 'card $F0B6F', 'card-bulleted $F0B70',
    'card-bulleted-off $F0B71', 'card-bulleted-off-outline $F0B72', 'card-bulleted-outline $F0B73',
    'card-bulleted-settings $F0B74', 'card-bulleted-settings-outline $F0B75', 'card-outline $F0B76',
    'card-text $F0B77', 'card-text-outline $F0B78', 'chat $F0B79',
    'chat-alert $F0B7A', 'chat-processing $F0B7B', 'chef-hat $F0B7C',
    'cloud-download-outline $F0B7D', 'cloud-upload-outline $F0B7E', 'coffin $F0B7F',
    'compass-off $F0B80', 'compass-off-outline $F0B81', 'controller-classic $F0B82',
    'controller-classic-outline $F0B83', 'cube-scan $F0B84', 'currency-brl $F0B85',
    'database-edit $F0B86', 'deathly-hallows $F0B87', 'delete-circle-outline $F0B88',
    'delete-forever-outline $F0B89', 'diamond $F0B8A', 'diamond-outline $F0B8B',
    'dns-outline $F0B8C', 'dots-horizontal-circle-outline $F0B8D', 'dots-vertical-circle-outline $F0B8E',
    'download-outline $F0B8F', 'drag-variant $F0B90', 'eject-outline $F0B91',
    'email-mark-as-unread $F0B92', 'export-variant $F0B93', 'eye-circle $F0B94',
    'eye-circle-outline $F0B95', 'face-man-outline $F0B96', 'file-find-outline $F0B97',
    'file-remove $F0B98', 'flag-minus $F0B99', 'flag-plus $F0B9A',
    'flag-remove $F0B9B', 'folder-account-outline $F0B9C', 'folder-plus-outline $F0B9D',
    'folder-remove-outline $F0B9E', 'folder-star-outline $F0B9F', 'gitlab $F0BA0',
    'gog $F0BA1', 'grave-stone $F0BA2', 'halloween $F0BA3',
    'hat-fedora $F0BA4', 'help-rhombus $F0BA5', 'help-rhombus-outline $F0BA6',
    'home-variant-outline $F0BA7', 'inbox-multiple-outline $F0BA8', 'library-shelves $F0BA9',
    'mapbox $F0BAA', 'menu-open $F0BAB', 'molecule $F0BAC',
    'one-up $F0BAD', 'open-source-initiative $F0BAE', 'pac-man $F0BAF',
    'page-next $F0BB0', 'page-next-outline $F0BB1', 'page-previous $F0BB2',
    'page-previous-outline $F0BB3', 'pan $F0BB4', 'pan-bottom-left $F0BB5',
    'pan-bottom-right $F0BB6', 'pan-down $F0BB7', 'pan-horizontal $F0BB8',
    'pan-left $F0BB9', 'pan-right $F0BBA', 'pan-top-left $F0BBB',
    'pan-top-right $F0BBC', 'pan-up $F0BBD', 'pan-vertical $F0BBE',
    'pumpkin $F0BBF', 'rollupjs $F0BC0', 'script $F0BC1',
    'script-text $F0BC2', 'script-text-outline $F0BC3', 'shield-key $F0BC4',
    'shield-key-outline $F0BC5', 'skull-crossbones $F0BC6', 'skull-crossbones-outline $F0BC7',
    'skull-outline $F0BC8', 'space-invaders $F0BC9', 'spider-web $F0BCA',
    'view-split-horizontal $F0BCB', 'view-split-vertical $F0BCC', 'swap-horizontal-bold $F0BCD',
    'swap-vertical-bold $F0BCE', 'tag-heart-outline $F0BCF', 'target-account $F0BD0',
    'timeline $F0BD1', 'timeline-outline $F0BD2', 'timeline-text $F0BD3',
    'timeline-text-outline $F0BD4', 'tooltip-image-outline $F0BD5', 'tooltip-plus $F0BD6',
    'tooltip-text-outline $F0BD7', 'train-car $F0BD8', 'triforce $F0BD9',
    'ubisoft $F0BDA', 'video-off-outline $F0BDB', 'video-outline $F0BDC',
    'wallet-outline $F0BDD', 'waze $F0BDE', 'wrap-disabled $F0BDF',
    'wrench-outline $F0BE0', 'access-point-network-off $F0BE1', 'account-check-outline $F0BE2',
    'account-heart-outline $F0BE3', 'account-key-outline $F0BE4', 'account-multiple-minus-outline $F0BE5',
    'account-network-outline $F0BE6', 'account-off-outline $F0BE7', 'account-star-outline $F0BE8',
    'airbag $F0BE9', 'alarm-light-outline $F0BEA', 'alpha-a-box-outline $F0BEB',
    'alpha-a-circle $F0BEC', 'alpha-a-circle-outline $F0BED', 'alpha-b-box-outline $F0BEE',
    'alpha-b-circle $F0BEF', 'alpha-b-circle-outline $F0BF0', 'alpha-c-box-outline $F0BF1',
    'alpha-c-circle $F0BF2', 'alpha-c-circle-outline $F0BF3', 'alpha-d-box-outline $F0BF4',
    'alpha-d-circle $F0BF5', 'alpha-d-circle-outline $F0BF6', 'alpha-e-box-outline $F0BF7',
    'alpha-e-circle $F0BF8', 'alpha-e-circle-outline $F0BF9', 'alpha-f-box-outline $F0BFA',
    'alpha-f-circle $F0BFB', 'alpha-f-circle-outline $F0BFC', 'alpha-g-box-outline $F0BFD',
    'alpha-g-circle $F0BFE', 'alpha-g-circle-outline $F0BFF', 'alpha-h-box-outline $F0C00',
    'alpha-h-circle $F0C01', 'alpha-h-circle-outline $F0C02', 'alpha-i-box-outline $F0C03',
    'alpha-i-circle $F0C04', 'alpha-i-circle-outline $F0C05', 'alpha-j-box-outline $F0C06',
    'alpha-j-circle $F0C07', 'alpha-j-circle-outline $F0C08', 'alpha-k-box-outline $F0C09',
    'alpha-k-circle $F0C0A', 'alpha-k-circle-outline $F0C0B', 'alpha-l-box-outline $F0C0C',
    'alpha-l-circle $F0C0D', 'alpha-l-circle-outline $F0C0E', 'alpha-m-box-outline $F0C0F',
    'alpha-m-circle $F0C10', 'alpha-m-circle-outline $F0C11', 'alpha-n-box-outline $F0C12',
    'alpha-n-circle $F0C13', 'alpha-n-circle-outline $F0C14', 'alpha-o-box-outline $F0C15',
    'alpha-o-circle $F0C16', 'alpha-o-circle-outline $F0C17', 'alpha-p-box-outline $F0C18',
    'alpha-p-circle $F0C19', 'alpha-p-circle-outline $F0C1A', 'alpha-q-box-outline $F0C1B',
    'alpha-q-circle $F0C1C', 'alpha-q-circle-outline $F0C1D', 'alpha-r-box-outline $F0C1E',
    'alpha-r-circle $F0C1F', 'alpha-r-circle-outline $F0C20', 'alpha-s-box-outline $F0C21',
    'alpha-s-circle $F0C22', 'alpha-s-circle-outline $F0C23', 'alpha-t-box-outline $F0C24',
    'alpha-t-circle $F0C25', 'alpha-t-circle-outline $F0C26', 'alpha-u-box-outline $F0C27',
    'alpha-u-circle $F0C28', 'alpha-u-circle-outline $F0C29', 'alpha-v-box-outline $F0C2A',
    'alpha-v-circle $F0C2B', 'alpha-v-circle-outline $F0C2C', 'alpha-w-box-outline $F0C2D',
    'alpha-w-circle $F0C2E', 'alpha-w-circle-outline $F0C2F', 'alpha-x-box-outline $F0C30',
    'alpha-x-circle $F0C31', 'alpha-x-circle-outline $F0C32', 'alpha-y-box-outline $F0C33',
    'alpha-y-circle $F0C34', 'alpha-y-circle-outline $F0C35', 'alpha-z-box-outline $F0C36',
    'alpha-z-circle $F0C37', 'alpha-z-circle-outline $F0C38', 'ballot-recount $F0C39',
    'ballot-recount-outline $F0C3A', 'basketball-hoop $F0C3B', 'basketball-hoop-outline $F0C3C',
    'briefcase-download-outline $F0C3D', 'briefcase-edit-outline $F0C3E', 'briefcase-minus-outline $F0C3F',
    'briefcase-plus-outline $F0C40', 'briefcase-remove-outline $F0C41', 'briefcase-search-outline $F0C42',
    'briefcase-upload-outline $F0C43', 'calendar-check-outline $F0C44', 'calendar-remove-outline $F0C45',
    'calendar-text-outline $F0C46', 'car-brake-abs $F0C47', 'car-brake-alert $F0C48',
    'car-esp $F0C49', 'car-light-dimmed $F0C4A', 'car-light-fog $F0C4B',
    'car-light-high $F0C4C', 'car-tire-alert $F0C4D', 'cart-arrow-right $F0C4E',
    'charity $F0C4F', 'chart-bell-curve $F0C50', 'checkbox-multiple-outline $F0C51',
    'checkbox-outline $F0C52', 'check-network $F0C53', 'check-network-outline $F0C54',
    'clipboard-account-outline $F0C55', 'clipboard-arrow-down-outline $F0C56', 'clipboard-arrow-up $F0C57',
    'clipboard-arrow-up-outline $F0C58', 'clipboard-play $F0C59', 'clipboard-play-outline $F0C5A',
    'clipboard-text-play $F0C5B', 'clipboard-text-play-outline $F0C5C', 'close-box-multiple $F0C5D',
    'close-box-multiple-outline $F0C5E', 'close-network-outline $F0C5F', 'console-network-outline $F0C60',
    'currency-ils $F0C61', 'delete-sweep-outline $F0C62', 'diameter $F0C63',
    'diameter-outline $F0C64', 'diameter-variant $F0C65', 'download-network-outline $F0C66',
    'dump-truck $F0C67', 'emoticon $F0C68', 'emoticon-angry $F0C69',
    'emoticon-angry-outline $F0C6A', 'emoticon-cool $F0C6B', 'emoticon-cry $F0C6C',
    'emoticon-cry-outline $F0C6D', 'emoticon-dead $F0C6E', 'emoticon-devil $F0C6F',
    'emoticon-excited $F0C70', 'emoticon-happy $F0C71', 'emoticon-kiss $F0C72',
    'emoticon-kiss-outline $F0C73', 'emoticon-neutral $F0C74', 'emoticon-poop-outline $F0C75',
    'emoticon-sad $F0C76', 'emoticon-tongue-outline $F0C77', 'emoticon-wink $F0C78',
    'emoticon-wink-outline $F0C79', 'eslint $F0C7A', 'face-recognition $F0C7B',
    'file-search $F0C7C', 'file-search-outline $F0C7D', 'file-table $F0C7E',
    'file-table-outline $F0C7F', 'folder-key-network-outline $F0C80', 'folder-network-outline $F0C81',
    'folder-text $F0C82', 'folder-text-outline $F0C83', 'food-apple-outline $F0C84',
    'fuse $F0C85', 'fuse-blade $F0C86', 'google-ads $F0C87',
    'google-street-view $F0C88', 'hazard-lights $F0C89', 'help-network-outline $F0C8A',
    'application-brackets $F0C8B', 'application-brackets-outline $F0C8C', 'image-size-select-actual $F0C8D',
    'image-size-select-large $F0C8E', 'image-size-select-small $F0C8F', 'ip-network-outline $F0C90',
    'ipod $F0C91', 'language-haskell $F0C92', 'leaf-maple $F0C93',
    'link-plus $F0C94', 'map-marker-check $F0C95', 'math-cos $F0C96',
    'math-sin $F0C97', 'math-tan $F0C98', 'microwave $F0C99',
    'minus-network-outline $F0C9A', 'network-off $F0C9B', 'network-off-outline $F0C9C',
    'network-outline $F0C9D', 'numeric-1-circle $F0CA0', 'numeric-1-circle-outline $F0CA1',
    'numeric-2-circle $F0CA2', 'numeric-2-circle-outline $F0CA3', 'numeric-3-circle $F0CA4',
    'numeric-3-circle-outline $F0CA5', 'numeric-4-circle $F0CA6', 'numeric-4-circle-outline $F0CA7',
    'numeric-5-circle $F0CA8', 'numeric-5-circle-outline $F0CA9', 'numeric-6-circle $F0CAA',
    'numeric-6-circle-outline $F0CAB', 'numeric-7-circle $F0CAC', 'numeric-7-circle-outline $F0CAD',
    'numeric-8-circle $F0CAE', 'numeric-8-circle-outline $F0CAF', 'numeric-9-circle $F0CB0',
    'numeric-9-circle-outline $F0CB1', 'numeric-9-plus-circle $F0CB2', 'numeric-9-plus-circle-outline $F0CB3',
    'parachute $F0CB4', 'parachute-outline $F0CB5', 'pencil-outline $F0CB6',
    'play-network-outline $F0CB7', 'playlist-music $F0CB8', 'playlist-music-outline $F0CB9',
    'plus-network-outline $F0CBA', 'postage-stamp $F0CBB', 'progress-alert $F0CBC',
    'progress-wrench $F0CBD', 'radio-am $F0CBE', 'radio-fm $F0CBF',
    'radius $F0CC0', 'radius-outline $F0CC1', 'ruler-square $F0CC2',
    'seat $F0CC3', 'seat-outline $F0CC4', 'seatbelt $F0CC5',
    'sheep $F0CC6', 'shield-airplane-outline $F0CC7', 'shield-check-outline $F0CC8',
    'shield-cross $F0CC9', 'shield-cross-outline $F0CCA', 'shield-home-outline $F0CCB',
    'shield-lock-outline $F0CCC', 'sort-variant-lock $F0CCD', 'sort-variant-lock-open $F0CCE',
    'source-repository $F0CCF', 'source-repository-multiple $F0CD0', 'spa $F0CD1',
    'spa-outline $F0CD2', 'toaster-oven $F0CD3', 'truck-check $F0CD4',
    'turnstile $F0CD5', 'turnstile-outline $F0CD6', 'turtle $F0CD7',
    'upload-network-outline $F0CD8', 'vibrate-off $F0CD9', 'watch-vibrate-off $F0CDA',
    'arrow-down-circle $F0CDB', 'arrow-down-circle-outline $F0CDC', 'arrow-left-circle $F0CDD',
    'arrow-left-circle-outline $F0CDE', 'arrow-right-circle $F0CDF', 'arrow-right-circle-outline $F0CE0',
    'arrow-up-circle $F0CE1', 'arrow-up-circle-outline $F0CE2', 'account-tie $F0CE3',
    'alert-box-outline $F0CE4', 'alert-decagram-outline $F0CE5', 'alert-octagon-outline $F0CE6',
    'alert-octagram-outline $F0CE7', 'ammunition $F0CE8', 'account-music-outline $F0CE9',
    'beaker $F0CEA', 'blender $F0CEB', 'blood-bag $F0CEC',
    'cross-bolnisi $F0CED', 'bread-slice $F0CEE', 'bread-slice-outline $F0CEF',
    'briefcase-account $F0CF0', 'briefcase-account-outline $F0CF1', 'brightness-percent $F0CF2',
    'bullet $F0CF3', 'cash-register $F0CF4', 'cross-celtic $F0CF5',
    'cross-outline $F0CF6', 'clipboard-alert-outline $F0CF7', 'clipboard-arrow-left-outline $F0CF8',
    'clipboard-arrow-right $F0CF9', 'clipboard-arrow-right-outline $F0CFA', 'content-save-edit $F0CFB',
    'content-save-edit-outline $F0CFC', 'cursor-default-click $F0CFD', 'cursor-default-click-outline $F0CFE',
    'database-sync $F0CFF', 'database-remove $F0D00', 'database-settings $F0D01',
    'drama-masks $F0D02', 'email-box $F0D03', 'eye-check $F0D04',
    'eye-check-outline $F0D05', 'fast-forward-30 $F0D06', 'order-alphabetical-descending $F0D07',
    'flower-poppy $F0D08', 'folder-pound $F0D09', 'folder-pound-outline $F0D0A',
    'folder-sync $F0D0B', 'folder-sync-outline $F0D0C', 'format-list-numbered-rtl $F0D0D',
    'format-text-wrapping-clip $F0D0E', 'format-text-wrapping-overflow $F0D0F', 'format-text-wrapping-wrap $F0D10',
    'format-textbox $F0D11', 'fountain-pen $F0D12', 'fountain-pen-tip $F0D13',
    'heart-broken-outline $F0D14', 'home-city $F0D15', 'home-city-outline $F0D16',
    'hubspot $F0D17', 'filmstrip-box-multiple $F0D18', 'play-box-multiple $F0D19',
    'link-box $F0D1A', 'link-box-outline $F0D1B', 'link-box-variant $F0D1C',
    'link-box-variant-outline $F0D1D', 'map-clock $F0D1E', 'map-clock-outline $F0D1F',
    'map-marker-path $F0D20', 'mother-nurse $F0D21', 'microsoft-outlook $F0D22',
    'perspective-less $F0D23', 'perspective-more $F0D24', 'podium $F0D25',
    'podium-bronze $F0D26', 'podium-gold $F0D27', 'podium-silver $F0D28',
    'quora $F0D29', 'rewind-10 $F0D2A', 'roller-skate $F0D2B',
    'rollerblade $F0D2C', 'language-ruby $F0D2D', 'sack $F0D2E',
    'sack-percent $F0D2F', 'safety-goggles $F0D30', 'select-color $F0D31',
    'selection-ellipse $F0D32', 'shield-link-variant $F0D33', 'shield-link-variant-outline $F0D34',
    'skate $F0D35', 'skew-less $F0D36', 'skew-more $F0D37',
    'speaker-multiple $F0D38', 'stamper $F0D39', 'tank $F0D3A',
    'tortoise $F0D3B', 'transit-connection $F0D3C', 'transit-connection-variant $F0D3D',
    'transmission-tower $F0D3E', 'weight-gram $F0D3F', 'youtube-subscription $F0D40',
    'zigbee $F0D41', 'email-alert-outline $F0D42', 'air-filter $F0D43',
    'air-purifier $F0D44', 'android-messages $F0D45', 'apps-box $F0D46',
    'atm $F0D47', 'axis $F0D48', 'axis-arrow $F0D49',
    'axis-arrow-lock $F0D4A', 'axis-lock $F0D4B', 'axis-x-arrow $F0D4C',
    'axis-x-arrow-lock $F0D4D', 'axis-x-rotate-clockwise $F0D4E', 'axis-x-rotate-counterclockwise $F0D4F',
    'axis-x-y-arrow-lock $F0D50', 'axis-y-arrow $F0D51', 'axis-y-arrow-lock $F0D52',
    'axis-y-rotate-clockwise $F0D53', 'axis-y-rotate-counterclockwise $F0D54', 'axis-z-arrow $F0D55',
    'axis-z-arrow-lock $F0D56', 'axis-z-rotate-clockwise $F0D57', 'axis-z-rotate-counterclockwise $F0D58',
    'bell-alert $F0D59', 'bell-circle $F0D5A', 'bell-circle-outline $F0D5B',
    'calendar-minus $F0D5C', 'camera-outline $F0D5D', 'car-brake-hold $F0D5E',
    'car-brake-parking $F0D5F', 'car-cruise-control $F0D60', 'car-defrost-front $F0D61',
    'car-defrost-rear $F0D62', 'car-parking-lights $F0D63', 'car-traction-control $F0D64',
    'bag-carry-on-check $F0D65', 'cart-arrow-down $F0D66', 'cart-arrow-up $F0D67',
    'cart-minus $F0D68', 'cart-remove $F0D69', 'contactless-payment $F0D6A',
    'creative-commons $F0D6B', 'credit-card-wireless-outline $F0D6C', 'cricket $F0D6D',
    'dev-to $F0D6E', 'domain-off $F0D6F', 'face-agent $F0D70',
    'fast-forward-10 $F0D71', 'flare $F0D72', 'format-text-rotation-down $F0D73',
    'format-text-rotation-none $F0D74', 'forwardburger $F0D75', 'gesture-swipe $F0D76',
    'gesture-tap-hold $F0D77', 'file-gif-box $F0D78', 'go-kart $F0D79',
    'go-kart-track $F0D7A', 'goodreads $F0D7B', 'grain $F0D7C',
    'hdr $F0D7D', 'hdr-off $F0D7E', 'hiking $F0D7F',
    'home-floor-1 $F0D80', 'home-floor-2 $F0D81', 'home-floor-3 $F0D82',
    'home-floor-a $F0D83', 'home-floor-b $F0D84', 'home-floor-g $F0D85',
    'home-floor-l $F0D86', 'kabaddi $F0D87', 'mailbox-open $F0D88',
    'mailbox-open-outline $F0D89', 'mailbox-open-up $F0D8A', 'mailbox-open-up-outline $F0D8B',
    'mailbox-outline $F0D8C', 'mailbox-up $F0D8D', 'mailbox-up-outline $F0D8E',
    'mixed-martial-arts $F0D8F', 'monitor-off $F0D90', 'motion-sensor $F0D91',
    'point-of-sale $F0D92', 'racing-helmet $F0D93', 'racquetball $F0D94',
    'restart-off $F0D95', 'rewind-30 $F0D96', 'room-service-outline $F0D97',
    'rotate-orbit $F0D98', 'rugby $F0D99', 'shield-search $F0D9A',
    'solar-panel $F0D9B', 'solar-panel-large $F0D9C', 'subway-alert-variant $F0D9D',
    'tea $F0D9E', 'tea-outline $F0D9F', 'tennis $F0DA0',
    'transfer-down $F0DA1', 'transfer-left $F0DA2', 'transfer-up $F0DA3',
    'trophy-broken $F0DA4', 'wind-turbine $F0DA5', 'wiper-wash $F0DA6',
    'badge-account $F0DA7', 'badge-account-alert $F0DA8', 'badge-account-alert-outline $F0DA9',
    'badge-account-outline $F0DAA', 'card-account-details-outline $F0DAB', 'air-horn $F0DAC',
    'application-export $F0DAD', 'application-import $F0DAE', 'bandage $F0DAF',
    'bank-minus $F0DB0', 'bank-plus $F0DB1', 'bank-remove $F0DB2',
    'bolt $F0DB3', 'bugle $F0DB4', 'cactus $F0DB5',
    'camera-wireless $F0DB6', 'camera-wireless-outline $F0DB7', 'cash-marker $F0DB8',
    'chevron-triple-down $F0DB9', 'chevron-triple-left $F0DBA', 'chevron-triple-right $F0DBB',
    'chevron-triple-up $F0DBC', 'closed-caption-outline $F0DBD', 'credit-card-marker-outline $F0DBE',
    'diving-flippers $F0DBF', 'diving-helmet $F0DC0', 'diving-scuba $F0DC1',
    'diving-scuba-flag $F0DC2', 'diving-scuba-tank $F0DC3', 'diving-scuba-tank-multiple $F0DC4',
    'diving-snorkel $F0DC5', 'file-cancel $F0DC6', 'file-cancel-outline $F0DC7',
    'file-document-edit $F0DC8', 'file-document-edit-outline $F0DC9', 'file-eye $F0DCA',
    'file-eye-outline $F0DCB', 'folder-alert $F0DCC', 'folder-alert-outline $F0DCD',
    'folder-edit-outline $F0DCE', 'folder-open-outline $F0DCF', 'format-list-bulleted-square $F0DD0',
    'gantry-crane $F0DD1', 'home-floor-0 $F0DD2', 'home-floor-negative-1 $F0DD3',
    'home-group $F0DD4', 'jabber $F0DD5', 'key-outline $F0DD6',
    'leak $F0DD7', 'leak-off $F0DD8', 'marker-cancel $F0DD9',
    'mine $F0DDA', 'monitor-lock $F0DDB', 'monitor-star $F0DDC',
    'movie-outline $F0DDD', 'music-note-plus $F0DDE', 'nail $F0DDF',
    'ocarina $F0DE0', 'passport-biometric $F0DE1', 'pen-lock $F0DE2',
    'pen-minus $F0DE3', 'pen-off $F0DE4', 'pen-plus $F0DE5',
    'pen-remove $F0DE6', 'pencil-lock-outline $F0DE7', 'pencil-minus $F0DE8',
    'pencil-minus-outline $F0DE9', 'pencil-off-outline $F0DEA', 'pencil-plus $F0DEB',
    'pencil-plus-outline $F0DEC', 'pencil-remove $F0DED', 'pencil-remove-outline $F0DEE',
    'phone-off $F0DEF', 'phone-outline $F0DF0', 'pi-hole $F0DF1',
    'playlist-star $F0DF2', 'screw-flat-top $F0DF3', 'screw-lag $F0DF4',
    'screw-machine-flat-top $F0DF5', 'screw-machine-round-top $F0DF6', 'screw-round-top $F0DF7',
    'send-circle $F0DF8', 'send-circle-outline $F0DF9', 'shoe-print $F0DFA',
    'signature $F0DFB', 'signature-freehand $F0DFC', 'signature-image $F0DFD',
    'signature-text $F0DFE', 'slope-downhill $F0DFF', 'slope-uphill $F0E00',
    'thermometer-alert $F0E01', 'thermometer-chevron-down $F0E02', 'thermometer-chevron-up $F0E03',
    'thermometer-minus $F0E04', 'thermometer-plus $F0E05', 'translate-off $F0E06',
    'upload-outline $F0E07', 'volume-variant-off $F0E08', 'wallpaper $F0E09',
    'water-outline $F0E0A', 'wifi-star $F0E0B', 'palette-outline $F0E0C',
    'badge-account-horizontal $F0E0D', 'badge-account-horizontal-outline $F0E0E', 'aws $F0E0F',
    'bag-personal $F0E10', 'bag-personal-off $F0E11', 'bag-personal-off-outline $F0E12',
    'bag-personal-outline $F0E13', 'biathlon $F0E14', 'bookmark-multiple $F0E15',
    'bookmark-multiple-outline $F0E16', 'calendar-month $F0E17', 'calendar-month-outline $F0E18',
    'camera-retake $F0E19', 'camera-retake-outline $F0E1A', 'car-back $F0E1B',
    'car-off $F0E1C', 'cast-education $F0E1D', 'check-bold $F0E1E',
    'check-underline $F0E1F', 'check-underline-circle $F0E20', 'check-underline-circle-outline $F0E21',
    'circular-saw $F0E22', 'comma $F0E23', 'comma-box-outline $F0E24',
    'comma-circle $F0E25', 'comma-circle-outline $F0E26', 'content-save-move $F0E27',
    'content-save-move-outline $F0E28', 'file-check-outline $F0E29', 'file-music-outline $F0E2A',
    'comma-box $F0E2B', 'file-video-outline $F0E2C', 'file-png-box $F0E2D',
    'fireplace $F0E2E', 'fireplace-off $F0E2F', 'firework $F0E30',
    'format-color-highlight $F0E31', 'format-text-variant $F0E32', 'gamepad-circle $F0E33',
    'gamepad-circle-down $F0E34', 'gamepad-circle-left $F0E35', 'gamepad-circle-outline $F0E36',
    'gamepad-circle-right $F0E37', 'gamepad-circle-up $F0E38', 'gamepad-down $F0E39',
    'gamepad-left $F0E3A', 'gamepad-right $F0E3B', 'gamepad-round $F0E3C',
    'gamepad-round-down $F0E3D', 'gamepad-round-left $F0E3E', 'gamepad-round-outline $F0E3F',
    'gamepad-round-right $F0E40', 'gamepad-round-up $F0E41', 'gamepad-up $F0E42',
    'gatsby $F0E43', 'gift $F0E44', 'grill $F0E45',
    'hand-back-left $F0E46', 'hand-back-right $F0E47', 'hand-saw $F0E48',
    'image-frame $F0E49', 'invert-colors-off $F0E4A', 'keyboard-off-outline $F0E4B',
    'layers-minus $F0E4C', 'layers-plus $F0E4D', 'layers-remove $F0E4E',
    'lightbulb-off $F0E4F', 'lightbulb-off-outline $F0E50', 'monitor-screenshot $F0E51',
    'ice-cream-off $F0E52', 'nfc-search-variant $F0E53', 'nfc-variant-off $F0E54',
    'notebook-multiple $F0E55', 'hoop-house $F0E56', 'picture-in-picture-bottom-right $F0E57',
    'picture-in-picture-bottom-right-outline $F0E58', 'picture-in-picture-top-right $F0E59', 'picture-in-picture-top-right-outline $F0E5A',
    'printer-3d-nozzle $F0E5B', 'printer-3d-nozzle-outline $F0E5C', 'printer-off $F0E5D',
    'rectangle $F0E5E', 'rectangle-outline $F0E5F', 'rivet $F0E60',
    'saw-blade $F0E61', 'seed $F0E62', 'seed-outline $F0E63',
    'signal-distance-variant $F0E64', 'spade $F0E65', 'sprout $F0E66',
    'sprout-outline $F0E67', 'table-tennis $F0E68', 'tree-outline $F0E69',
    'view-comfy $F0E6A', 'view-compact $F0E6B', 'view-compact-outline $F0E6C',
    'vuetify $F0E6D', 'weather-cloudy-arrow-right $F0E6E', 'microsoft-xbox-controller-menu $F0E6F',
    'microsoft-xbox-controller-view $F0E70', 'alarm-note $F0E71', 'alarm-note-off $F0E72',
    'arrow-left-right $F0E73', 'arrow-left-right-bold $F0E74', 'arrow-top-left-bottom-right $F0E75',
    'arrow-top-left-bottom-right-bold $F0E76', 'arrow-top-right-bottom-left $F0E77', 'arrow-top-right-bottom-left-bold $F0E78',
    'arrow-up-down $F0E79', 'arrow-up-down-bold $F0E7A', 'atom-variant $F0E7B',
    'baby-face $F0E7C', 'baby-face-outline $F0E7D', 'backspace-reverse $F0E7E',
    'backspace-reverse-outline $F0E7F', 'bank-outline $F0E80', 'bell-alert-outline $F0E81',
    'book-play $F0E82', 'book-play-outline $F0E83', 'book-search $F0E84',
    'book-search-outline $F0E85', 'boom-gate $F0E86', 'boom-gate-alert $F0E87',
    'boom-gate-alert-outline $F0E88', 'boom-gate-arrow-down $F0E89', 'boom-gate-arrow-down-outline $F0E8A',
    'boom-gate-outline $F0E8B', 'boom-gate-arrow-up $F0E8C', 'boom-gate-arrow-up-outline $F0E8D',
    'calendar-sync $F0E8E', 'calendar-sync-outline $F0E8F', 'cellphone-nfc $F0E90',
    'chart-areaspline-variant $F0E91', 'chart-scatter-plot $F0E92', 'chart-timeline-variant $F0E93',
    'chart-tree $F0E94', 'circle-double $F0E95', 'circle-expand $F0E96',
    'clock-digital $F0E97', 'card-account-mail-outline $F0E98', 'card-account-phone $F0E99',
    'card-account-phone-outline $F0E9A', 'account-cowboy-hat $F0E9B', 'currency-rial $F0E9C',
    'delete-empty-outline $F0E9D', 'dolly $F0E9E', 'electric-switch $F0E9F',
    'ellipse $F0EA0', 'ellipse-outline $F0EA1', 'equalizer $F0EA2',
    'equalizer-outline $F0EA3', 'ferris-wheel $F0EA4', 'file-delimited-outline $F0EA5',
    'text-box-check $F0EA6', 'text-box-check-outline $F0EA7', 'text-box-minus $F0EA8',
    'text-box-minus-outline $F0EA9', 'text-box-plus $F0EAA', 'text-box-plus-outline $F0EAB',
    'text-box-remove $F0EAC', 'text-box-remove-outline $F0EAD', 'text-box-search $F0EAE',
    'text-box-search-outline $F0EAF', 'file-image-outline $F0EB0', 'fingerprint-off $F0EB1',
    'format-list-bulleted-triangle $F0EB2', 'format-overline $F0EB3', 'frequently-asked-questions $F0EB4',
    'gamepad-square $F0EB5', 'gamepad-square-outline $F0EB6', 'gamepad-variant-outline $F0EB7',
    'gas-station-outline $F0EB8', 'google-podcast $F0EB9', 'home-analytics $F0EBA',
    'mail $F0EBB', 'map-check $F0EBC', 'map-check-outline $F0EBD',
    'ruler-square-compass $F0EBE', 'notebook-outline $F0EBF', 'penguin $F0EC0',
    'radioactive-off $F0EC1', 'record-circle $F0EC2', 'record-circle-outline $F0EC3',
    'remote-off $F0EC4', 'remote-tv $F0EC5', 'remote-tv-off $F0EC6',
    'rotate-3d $F0EC7', 'sail-boat $F0EC8', 'scatter-plot $F0EC9',
    'scatter-plot-outline $F0ECA', 'segment $F0ECB', 'shield-alert $F0ECC',
    'shield-alert-outline $F0ECD', 'tablet-dashboard $F0ECE', 'television-play $F0ECF',
    'unicode $F0ED0', 'video-3d-variant $F0ED1', 'video-wireless $F0ED2',
    'video-wireless-outline $F0ED3', 'account-voice-off $F0ED4', 'bacteria $F0ED5',
    'bacteria-outline $F0ED6', 'calendar-account $F0ED7', 'calendar-account-outline $F0ED8',
    'calendar-weekend $F0ED9', 'calendar-weekend-outline $F0EDA', 'camera-plus $F0EDB',
    'camera-plus-outline $F0EDC', 'campfire $F0EDD', 'chat-outline $F0EDE',
    'cpu-32-bit $F0EDF', 'cpu-64-bit $F0EE0', 'credit-card-clock $F0EE1',
    'credit-card-clock-outline $F0EE2', 'email-edit $F0EE3', 'email-edit-outline $F0EE4',
    'email-minus $F0EE5', 'email-minus-outline $F0EE6', 'email-multiple $F0EE7',
    'email-multiple-outline $F0EE8', 'email-open-multiple $F0EE9', 'email-open-multiple-outline $F0EEA',
    'file-cad $F0EEB', 'file-cad-box $F0EEC', 'file-plus-outline $F0EED',
    'filter-minus $F0EEE', 'filter-minus-outline $F0EEF', 'filter-plus $F0EF0',
    'filter-plus-outline $F0EF1', 'fire-extinguisher $F0EF2', 'fishbowl $F0EF3',
    'fishbowl-outline $F0EF4', 'fit-to-page $F0EF5', 'fit-to-page-outline $F0EF6',
    'flash-alert $F0EF7', 'flash-alert-outline $F0EF8', 'heart-flash $F0EF9',
    'home-flood $F0EFA', 'human-male-height $F0EFB', 'human-male-height-variant $F0EFC',
    'ice-pop $F0EFD', 'identifier $F0EFE', 'image-filter-center-focus-strong $F0EFF',
    'image-filter-center-focus-strong-outline $F0F00', 'jellyfish $F0F01', 'jellyfish-outline $F0F02',
    'lasso $F0F03', 'music-box-multiple-outline $F0F04', 'map-marker-alert $F0F05',
    'map-marker-alert-outline $F0F06', 'map-marker-question $F0F07', 'map-marker-question-outline $F0F08',
    'map-marker-remove $F0F09', 'map-marker-remove-variant $F0F0A', 'necklace $F0F0B',
    'newspaper-minus $F0F0C', 'newspaper-plus $F0F0D', 'numeric-0-box-multiple $F0F0E',
    'numeric-1-box-multiple $F0F0F', 'numeric-2-box-multiple $F0F10', 'numeric-3-box-multiple $F0F11',
    'numeric-4-box-multiple $F0F12', 'numeric-5-box-multiple $F0F13', 'numeric-6-box-multiple $F0F14',
    'numeric-7-box-multiple $F0F15', 'numeric-8-box-multiple $F0F16', 'numeric-9-box-multiple $F0F17',
    'numeric-9-plus-box-multiple $F0F18', 'oil-lamp $F0F19', 'phone-alert $F0F1A',
    'play-outline $F0F1B', 'purse $F0F1C', 'purse-outline $F0F1D',
    'railroad-light $F0F1E', 'reply-all-outline $F0F1F', 'reply-outline $F0F20',
    'rss-off $F0F21', 'selection-ellipse-arrow-inside $F0F22', 'share-off $F0F23',
    'share-off-outline $F0F24', 'skip-backward-outline $F0F25', 'skip-forward-outline $F0F26',
    'skip-next-outline $F0F27', 'skip-previous-outline $F0F28', 'snowflake-alert $F0F29',
    'snowflake-variant $F0F2A', 'stretch-to-page $F0F2B', 'stretch-to-page-outline $F0F2C',
    'typewriter $F0F2D', 'wave $F0F2E', 'weather-cloudy-alert $F0F2F',
    'weather-hazy $F0F30', 'weather-night-partly-cloudy $F0F31', 'weather-partly-lightning $F0F32',
    'weather-partly-rainy $F0F33', 'weather-partly-snowy $F0F34', 'weather-partly-snowy-rainy $F0F35',
    'weather-snowy-heavy $F0F36', 'weather-sunny-alert $F0F37', 'weather-tornado $F0F38',
    'baby-bottle $F0F39', 'baby-bottle-outline $F0F3A', 'bag-carry-on $F0F3B',
    'bag-carry-on-off $F0F3C', 'bag-checked $F0F3D', 'baguette $F0F3E',
    'bus-multiple $F0F3F', 'car-shift-pattern $F0F40', 'cellphone-information $F0F41',
    'content-save-alert $F0F42', 'content-save-alert-outline $F0F43', 'content-save-all-outline $F0F44',
    'crosshairs-off $F0F45', 'cupboard $F0F46', 'cupboard-outline $F0F47',
    'chair-rolling $F0F48', 'draw $F0F49', 'dresser $F0F4A',
    'dresser-outline $F0F4B', 'emoticon-frown $F0F4C', 'emoticon-frown-outline $F0F4D',
    'focus-auto $F0F4E', 'focus-field $F0F4F', 'focus-field-horizontal $F0F50',
    'focus-field-vertical $F0F51', 'foot-print $F0F52', 'handball $F0F53',
    'home-thermometer $F0F54', 'home-thermometer-outline $F0F55', 'kettle-outline $F0F56',
    'latitude $F0F57', 'layers-triple $F0F58', 'layers-triple-outline $F0F59',
    'longitude $F0F5A', 'language-markdown-outline $F0F5B', 'merge $F0F5C',
    'middleware $F0F5D', 'middleware-outline $F0F5E', 'monitor-speaker $F0F5F',
    'monitor-speaker-off $F0F60', 'moon-first-quarter $F0F61', 'moon-full $F0F62',
    'moon-last-quarter $F0F63', 'moon-new $F0F64', 'moon-waning-crescent $F0F65',
    'moon-waning-gibbous $F0F66', 'moon-waxing-crescent $F0F67', 'moon-waxing-gibbous $F0F68',
    'music-accidental-double-flat $F0F69', 'music-accidental-double-sharp $F0F6A', 'music-accidental-flat $F0F6B',
    'music-accidental-natural $F0F6C', 'music-accidental-sharp $F0F6D', 'music-clef-alto $F0F6E',
    'music-clef-bass $F0F6F', 'music-clef-treble $F0F70', 'music-note-eighth-dotted $F0F71',
    'music-note-half-dotted $F0F72', 'music-note-off-outline $F0F73', 'music-note-outline $F0F74',
    'music-note-quarter-dotted $F0F75', 'music-note-sixteenth-dotted $F0F76', 'music-note-whole-dotted $F0F77',
    'music-rest-eighth $F0F78', 'music-rest-half $F0F79', 'music-rest-quarter $F0F7A',
    'music-rest-sixteenth $F0F7B', 'music-rest-whole $F0F7C', 'numeric-10-box $F0F7D',
    'numeric-10-box-outline $F0F7E', 'page-layout-header-footer $F0F7F', 'patio-heater $F0F80',
    'warehouse $F0F81', 'select-group $F0F82', 'shield-car $F0F83',
    'shopping-search $F0F84', 'speedometer-medium $F0F85', 'speedometer-slow $F0F86',
    'table-large-plus $F0F87', 'table-large-remove $F0F88', 'television-pause $F0F89',
    'television-stop $F0F8A', 'transit-detour $F0F8B', 'video-input-scart $F0F8C',
    'view-grid-plus $F0F8D', 'wallet-plus $F0F8E', 'wallet-plus-outline $F0F8F',
    'wardrobe $F0F90', 'wardrobe-outline $F0F91', 'water-boiler $F0F92',
    'water-pump-off $F0F93', 'web-box $F0F94', 'timeline-alert $F0F95',
    'timeline-plus $F0F96', 'timeline-plus-outline $F0F97', 'timeline-alert-outline $F0F98',
    'timeline-help $F0F99', 'timeline-help-outline $F0F9A', 'home-export-outline $F0F9B',
    'home-import-outline $F0F9C', 'account-filter-outline $F0F9D', 'approximately-equal $F0F9E',
    'approximately-equal-box $F0F9F', 'baby-carriage-off $F0FA0', 'bee $F0FA1',
    'bee-flower $F0FA2', 'car-child-seat $F0FA3', 'car-seat $F0FA4',
    'car-seat-cooler $F0FA5', 'car-seat-heater $F0FA6', 'chart-bell-curve-cumulative $F0FA7',
    'clock-check $F0FA8', 'clock-check-outline $F0FA9', 'coffee-off $F0FAA',
    'coffee-off-outline $F0FAB', 'credit-card-minus $F0FAC', 'credit-card-minus-outline $F0FAD',
    'credit-card-remove $F0FAE', 'credit-card-remove-outline $F0FAF', 'devices $F0FB0',
    'email-newsletter $F0FB1', 'expansion-card-variant $F0FB2', 'power-socket-ch $F0FB3',
    'file-swap $F0FB4', 'file-swap-outline $F0FB5', 'folder-swap $F0FB6',
    'folder-swap-outline $F0FB7', 'format-letter-ends-with $F0FB8', 'format-letter-matches $F0FB9',
    'format-letter-starts-with $F0FBA', 'format-text-rotation-angle-down $F0FBB', 'format-text-rotation-angle-up $F0FBC',
    'format-text-rotation-down-vertical $F0FBD', 'format-text-rotation-up $F0FBE', 'format-text-rotation-vertical $F0FBF',
    'id-card $F0FC0', 'image-auto-adjust $F0FC1', 'key-wireless $F0FC2',
    'license $F0FC3', 'location-enter $F0FC4', 'location-exit $F0FC5',
    'lock-open-variant $F0FC6', 'lock-open-variant-outline $F0FC7', 'math-integral $F0FC8',
    'math-integral-box $F0FC9', 'math-norm $F0FCA', 'math-norm-box $F0FCB',
    'message-lock $F0FCC', 'message-text-lock $F0FCD', 'movie-open $F0FCE',
    'movie-open-outline $F0FCF', 'bed-queen $F0FD0', 'bed-king-outline $F0FD1',
    'bed-king $F0FD2', 'bed-double-outline $F0FD3', 'bed-double $F0FD4',
    'microsoft-azure-devops $F0FD5', 'arm-flex-outline $F0FD6', 'arm-flex $F0FD7',
    'protocol $F0FD8', 'seal-variant $F0FD9', 'select-place $F0FDA',
    'bed-queen-outline $F0FDB', 'sign-direction-plus $F0FDC', 'sign-direction-remove $F0FDD',
    'silverware-clean $F0FDE', 'slash-forward $F0FDF', 'slash-forward-box $F0FE0',
    'swap-horizontal-circle $F0FE1', 'swap-horizontal-circle-outline $F0FE2', 'swap-vertical-circle $F0FE3',
    'swap-vertical-circle-outline $F0FE4', 'tanker-truck $F0FE5', 'texture-box $F0FE6',
    'tram-side $F0FE7', 'vector-link $F0FE8', 'numeric-10 $F0FE9',
    'numeric-10-box-multiple $F0FEA', 'numeric-10-box-multiple-outline $F0FEB', 'numeric-10-circle $F0FEC',
    'numeric-10-circle-outline $F0FED', 'numeric-9-plus $F0FEE', 'credit-card $F0FEF',
    'credit-card-multiple $F0FF0', 'credit-card-off $F0FF1', 'credit-card-plus $F0FF2',
    'credit-card-refund $F0FF3', 'credit-card-scan $F0FF4', 'credit-card-settings $F0FF5',
    'hospital $F0FF6', 'hospital-box-outline $F0FF7', 'oil-temperature $F0FF8',
    'stadium $F0FF9', 'zip-box-outline $F0FFA', 'account-edit-outline $F0FFB',
    'peanut $F0FFC', 'peanut-off $F0FFD', 'peanut-outline $F0FFE',
    'peanut-off-outline $F0FFF', 'sign-direction-minus $F1000', 'newspaper-variant $F1001',
    'newspaper-variant-multiple $F1002', 'newspaper-variant-multiple-outline $F1003', 'newspaper-variant-outline $F1004',
    'overscan $F1005', 'pig-variant $F1006', 'piggy-bank $F1007',
    'post $F1008', 'post-outline $F1009', 'account-box-multiple-outline $F100A',
    'airballoon-outline $F100B', 'alphabetical-off $F100C', 'alphabetical-variant $F100D',
    'alphabetical-variant-off $F100E', 'apache-kafka $F100F', 'billboard $F1010',
    'blinds-open $F1011', 'bus-stop $F1012', 'bus-stop-covered $F1013',
    'bus-stop-uncovered $F1014', 'car-2-plus $F1015', 'car-3-plus $F1016',
    'car-brake-retarder $F1017', 'car-clutch $F1018', 'car-coolant-level $F1019',
    'car-turbocharger $F101A', 'car-windshield $F101B', 'car-windshield-outline $F101C',
    'cards-diamond-outline $F101D', 'cast-audio $F101E', 'cellphone-play $F101F',
    'coach-lamp $F1020', 'comment-quote $F1021', 'comment-quote-outline $F1022',
    'domino-mask $F1023', 'electron-framework $F1024', 'excavator $F1025',
    'eye-minus $F1026', 'eye-minus-outline $F1027', 'file-account-outline $F1028',
    'file-chart-outline $F1029', 'file-cloud-outline $F102A', 'file-code-outline $F102B',
    'file-excel-box-outline $F102C', 'file-excel-outline $F102D', 'file-export-outline $F102E',
    'file-import-outline $F102F', 'file-lock-outline $F1030', 'file-move-outline $F1031',
    'file-multiple-outline $F1032', 'file-percent-outline $F1033', 'file-powerpoint-box-outline $F1034',
    'file-powerpoint-outline $F1035', 'file-question-outline $F1036', 'file-remove-outline $F1037',
    'file-restore-outline $F1038', 'file-send-outline $F1039', 'file-star $F103A',
    'file-star-outline $F103B', 'file-undo-outline $F103C', 'file-word-box-outline $F103D',
    'file-word-outline $F103E', 'filter-variant-remove $F103F', 'floor-lamp-dual $F1040',
    'floor-lamp-torchiere-variant $F1041', 'fruit-cherries $F1042', 'fruit-citrus $F1043',
    'fruit-grapes $F1044', 'fruit-grapes-outline $F1045', 'fruit-pineapple $F1046',
    'fruit-watermelon $F1047', 'google-my-business $F1048', 'graph $F1049',
    'graph-outline $F104A', 'harddisk-plus $F104B', 'harddisk-remove $F104C',
    'home-circle-outline $F104D', 'instrument-triangle $F104E', 'island $F104F',
    'keyboard-space $F1050', 'led-strip-variant $F1051', 'numeric-negative-1 $F1052',
    'oil-level $F1053', 'outdoor-lamp $F1054', 'palm-tree $F1055',
    'party-popper $F1056', 'printer-pos $F1057', 'robber $F1058',
    'routes-clock $F1059', 'scale-off $F105A', 'cog-transfer $F105B',
    'cog-transfer-outline $F105C', 'shield-sun $F105D', 'shield-sun-outline $F105E',
    'sprinkler $F105F', 'sprinkler-variant $F1060', 'table-chair $F1061',
    'terraform $F1062', 'toaster $F1063', 'tools $F1064',
    'transfer $F1065', 'valve $F1066', 'valve-closed $F1067',
    'valve-open $F1068', 'video-check $F1069', 'video-check-outline $F106A',
    'water-well $F106B', 'water-well-outline $F106C', 'bed-single $F106D',
    'bed-single-outline $F106E', 'book-information-variant $F106F', 'bottle-soda $F1070',
    'bottle-soda-classic $F1071', 'bottle-soda-outline $F1072', 'calendar-blank-multiple $F1073',
    'card-search $F1074', 'card-search-outline $F1075', 'face-woman-profile $F1076',
    'face-woman $F1077', 'face-woman-outline $F1078', 'file-settings $F1079',
    'file-settings-outline $F107A', 'file-cog $F107B', 'file-cog-outline $F107C',
    'folder-settings $F107D', 'folder-settings-outline $F107E', 'folder-cog $F107F',
    'folder-cog-outline $F1080', 'furigana-horizontal $F1081', 'furigana-vertical $F1082',
    'golf-tee $F1083', 'lungs $F1084', 'math-log $F1085',
    'moped $F1086', 'router-network $F1087', 'roman-numeral-2 $F1089',
    'roman-numeral-3 $F108A', 'roman-numeral-4 $F108B', 'roman-numeral-6 $F108D',
    'roman-numeral-7 $F108E', 'roman-numeral-8 $F108F', 'roman-numeral-9 $F1090',
    'soldering-iron $F1092', 'stomach $F1093', 'table-eye $F1094',
    'form-textarea $F1095', 'trumpet $F1096', 'account-cash $F1097',
    'account-cash-outline $F1098', 'air-humidifier $F1099', 'ansible $F109A',
    'api $F109B', 'bicycle $F109C', 'car-door-lock $F109D',
    'coat-rack $F109E', 'coffee-maker $F109F', 'web-minus $F10A0',
    'decimal $F10A1', 'decimal-comma $F10A2', 'decimal-comma-decrease $F10A3',
    'decimal-comma-increase $F10A4', 'delete-alert $F10A5', 'delete-alert-outline $F10A6',
    'delete-off $F10A7', 'delete-off-outline $F10A8', 'dock-bottom $F10A9',
    'dock-left $F10AA', 'dock-right $F10AB', 'dock-window $F10AC',
    'domain-plus $F10AD', 'domain-remove $F10AE', 'door-closed-lock $F10AF',
    'download-off $F10B0', 'download-off-outline $F10B1', 'flag-minus-outline $F10B2',
    'flag-plus-outline $F10B3', 'flag-remove-outline $F10B4', 'folder-home $F10B5',
    'folder-home-outline $F10B6', 'folder-information $F10B7', 'folder-information-outline $F10B8',
    'iv-bag $F10B9', 'link-lock $F10BA', 'message-plus-outline $F10BB',
    'phone-cancel $F10BC', 'smart-card $F10BD', 'smart-card-outline $F10BE',
    'smart-card-reader $F10BF', 'smart-card-reader-outline $F10C0', 'storefront-outline $F10C1',
    'thermometer-high $F10C2', 'thermometer-low $F10C3', 'ufo $F10C4',
    'ufo-outline $F10C5', 'upload-off $F10C6', 'upload-off-outline $F10C7',
    'account-child-outline $F10C8', 'account-settings-outline $F10C9', 'account-tie-outline $F10CA',
    'alien-outline $F10CB', 'battery-alert-variant $F10CC', 'battery-alert-variant-outline $F10CD',
    'beehive-outline $F10CE', 'boomerang $F10CF', 'briefcase-clock $F10D0',
    'briefcase-clock-outline $F10D1', 'cellphone-message-off $F10D2', 'circle-off-outline $F10D3',
    'clipboard-list $F10D4', 'clipboard-list-outline $F10D5', 'code-braces-box $F10D6',
    'code-parentheses-box $F10D7', 'consolidate $F10D8', 'electric-switch-closed $F10D9',
    'email-receive $F10DA', 'email-receive-outline $F10DB', 'email-send $F10DC',
    'email-send-outline $F10DD', 'emoticon-confused $F10DE', 'emoticon-confused-outline $F10DF',
    'epsilon $F10E0', 'file-table-box $F10E1', 'file-table-box-multiple $F10E2',
    'file-table-box-multiple-outline $F10E3', 'file-table-box-outline $F10E4', 'filter-menu $F10E5',
    'filter-menu-outline $F10E6', 'flip-horizontal $F10E7', 'flip-vertical $F10E8',
    'folder-download-outline $F10E9', 'folder-heart $F10EA', 'folder-heart-outline $F10EB',
    'folder-key-outline $F10EC', 'folder-upload-outline $F10ED', 'gamma $F10EE',
    'hair-dryer $F10EF', 'hair-dryer-outline $F10F0', 'hand-heart $F10F1',
    'hexagon-multiple-outline $F10F2', 'horizontal-rotate-clockwise $F10F3', 'horizontal-rotate-counterclockwise $F10F4',
    'application-array $F10F5', 'application-array-outline $F10F6', 'application-braces $F10F7',
    'application-braces-outline $F10F8', 'application-parentheses $F10F9', 'application-parentheses-outline $F10FA',
    'application-variable $F10FB', 'application-variable-outline $F10FC', 'khanda $F10FD',
    'kubernetes $F10FE', 'link-variant-minus $F10FF', 'link-variant-plus $F1100',
    'link-variant-remove $F1101', 'map-marker-down $F1102', 'map-marker-up $F1103',
    'monitor-shimmer $F1104', 'nix $F1105', 'nuxt $F1106',
    'power-socket-de $F1107', 'power-socket-fr $F1108', 'power-socket-jp $F1109',
    'progress-close $F110A', 'reload-alert $F110B', 'restart-alert $F110C',
    'restore-alert $F110D', 'shaker $F110E', 'shaker-outline $F110F',
    'television-shimmer $F1110', 'variable-box $F1111', 'filter-variant-minus $F1112',
    'filter-variant-plus $F1113', 'slot-machine $F1114', 'slot-machine-outline $F1115',
    'glass-mug-variant $F1116', 'clipboard-flow-outline $F1117', 'sign-real-estate $F1118',
    'antenna $F1119', 'centos $F111A', 'redhat $F111B',
    'window-shutter $F111C', 'window-shutter-alert $F111D', 'window-shutter-open $F111E',
    'bike-fast $F111F', 'volume-source $F1120', 'volume-vibrate $F1121',
    'movie-edit $F1122', 'movie-edit-outline $F1123', 'movie-filter $F1124',
    'movie-filter-outline $F1125', 'diabetes $F1126', 'cursor-default-gesture $F1127',
    'cursor-default-gesture-outline $F1128', 'toothbrush $F1129', 'toothbrush-paste $F112A',
    'home-roof $F112B', 'toothbrush-electric $F112C', 'account-supervisor-outline $F112D',
    'bottle-tonic $F112E', 'bottle-tonic-outline $F112F', 'bottle-tonic-plus $F1130',
    'bottle-tonic-plus-outline $F1131', 'bottle-tonic-skull $F1132', 'bottle-tonic-skull-outline $F1133',
    'calendar-arrow-left $F1134', 'calendar-arrow-right $F1135', 'crosshairs-question $F1136',
    'fire-hydrant $F1137', 'fire-hydrant-alert $F1138', 'fire-hydrant-off $F1139',
    'ocr $F113A', 'shield-star $F113B', 'shield-star-outline $F113C',
    'text-recognition $F113D', 'handcuffs $F113E', 'gender-male-female-variant $F113F',
    'gender-non-binary $F1140', 'minus-box-multiple $F1141', 'minus-box-multiple-outline $F1142',
    'plus-box-multiple-outline $F1143', 'pencil-box-multiple $F1144', 'pencil-box-multiple-outline $F1145',
    'printer-check $F1146', 'sort-variant-remove $F1147', 'sort-alphabetical-ascending-variant $F1148',
    'sort-alphabetical-descending-variant $F1149', 'dice-1-outline $F114A', 'dice-2-outline $F114B',
    'dice-3-outline $F114C', 'dice-4-outline $F114D', 'dice-5-outline $F114E',
    'dice-6-outline $F114F', 'dice-d4 $F1150', 'dice-d6 $F1151',
    'dice-d8 $F1152', 'dice-d10 $F1153', 'dice-d12 $F1154',
    'dice-d20 $F1155', 'dice-multiple-outline $F1156', 'paper-roll $F1157',
    'paper-roll-outline $F1158', 'home-edit $F1159', 'home-edit-outline $F115A',
    'arrow-horizontal-lock $F115B', 'arrow-vertical-lock $F115C', 'weight-lifter $F115D',
    'account-lock $F115E', 'account-lock-outline $F115F', 'pasta $F1160',
    'send-check $F1161', 'send-check-outline $F1162', 'send-clock $F1163',
    'send-clock-outline $F1164', 'send-outline $F1165', 'send-lock-outline $F1166',
    'police-badge $F1167', 'police-badge-outline $F1168', 'gate-arrow-right $F1169',
    'gate-open $F116A', 'bell-badge $F116B', 'message-image-outline $F116C',
    'message-lock-outline $F116D', 'message-minus $F116E', 'message-minus-outline $F116F',
    'message-processing-outline $F1170', 'message-settings-outline $F1171', 'message-cog-outline $F1172',
    'message-text-clock $F1173', 'message-text-clock-outline $F1174', 'message-text-lock-outline $F1175',
    'checkbox-blank-badge $F1176', 'file-link $F1177', 'file-link-outline $F1178',
    'file-phone $F1179', 'file-phone-outline $F117A', 'meditation $F117B',
    'yoga $F117C', 'leek $F117D', 'noodles $F117E',
    'pound-box-outline $F117F', 'school-outline $F1180', 'basket-outline $F1181',
    'phone-in-talk-outline $F1182', 'bash $F1183', 'file-key $F1184',
    'file-key-outline $F1185', 'file-certificate $F1186', 'file-certificate-outline $F1187',
    'certificate-outline $F1188', 'cigar $F1189', 'grill-outline $F118A',
    'qrcode-plus $F118B', 'qrcode-minus $F118C', 'qrcode-remove $F118D',
    'phone-alert-outline $F118E', 'phone-bluetooth-outline $F118F', 'phone-cancel-outline $F1190',
    'phone-forward-outline $F1191', 'phone-hangup-outline $F1192', 'phone-incoming-outline $F1193',
    'phone-lock-outline $F1194', 'phone-log-outline $F1195', 'phone-message $F1196',
    'phone-message-outline $F1197', 'phone-minus-outline $F1198', 'phone-outgoing-outline $F1199',
    'phone-paused-outline $F119A', 'phone-plus-outline $F119B', 'phone-return-outline $F119C',
    'phone-settings-outline $F119D', 'key-star $F119E', 'key-link $F119F',
    'shield-edit $F11A0', 'shield-edit-outline $F11A1', 'shield-sync $F11A2',
    'shield-sync-outline $F11A3', 'golf-cart $F11A4', 'phone-missed-outline $F11A5',
    'phone-off-outline $F11A6', 'format-quote-open-outline $F11A7', 'format-quote-close-outline $F11A8',
    'phone-check $F11A9', 'phone-check-outline $F11AA', 'phone-ring $F11AB',
    'phone-ring-outline $F11AC', 'share-circle $F11AD', 'reply-circle $F11AE',
    'fridge-off $F11AF', 'fridge-off-outline $F11B0', 'fridge-alert $F11B1',
    'fridge-alert-outline $F11B2', 'water-boiler-alert $F11B3', 'water-boiler-off $F11B4',
    'amplifier-off $F11B5', 'audio-video-off $F11B6', 'toaster-off $F11B7',
    'dishwasher-alert $F11B8', 'dishwasher-off $F11B9', 'tumble-dryer-alert $F11BA',
    'tumble-dryer-off $F11BB', 'washing-machine-alert $F11BC', 'washing-machine-off $F11BD',
    'car-info $F11BE', 'comment-edit $F11BF', 'printer-3d-nozzle-alert $F11C0',
    'printer-3d-nozzle-alert-outline $F11C1', 'align-horizontal-left $F11C2', 'align-horizontal-center $F11C3',
    'align-horizontal-right $F11C4', 'align-vertical-bottom $F11C5', 'align-vertical-center $F11C6',
    'align-vertical-top $F11C7', 'distribute-horizontal-left $F11C8', 'distribute-horizontal-center $F11C9',
    'distribute-horizontal-right $F11CA', 'distribute-vertical-bottom $F11CB', 'distribute-vertical-center $F11CC',
    'distribute-vertical-top $F11CD', 'alert-rhombus $F11CE', 'alert-rhombus-outline $F11CF',
    'crown-outline $F11D0', 'image-off-outline $F11D1', 'movie-search $F11D2',
    'movie-search-outline $F11D3', 'rv-truck $F11D4', 'shopping-outline $F11D5',
    'strategy $F11D6', 'note-text-outline $F11D7', 'view-agenda-outline $F11D8',
    'view-grid-outline $F11D9', 'view-grid-plus-outline $F11DA', 'window-closed-variant $F11DB',
    'window-open-variant $F11DC', 'cog-clockwise $F11DD', 'cog-counterclockwise $F11DE',
    'chart-sankey $F11DF', 'chart-sankey-variant $F11E0', 'vanity-light $F11E1',
    'router $F11E2', 'image-edit $F11E3', 'image-edit-outline $F11E4',
    'bell-check $F11E5', 'bell-check-outline $F11E6', 'file-edit $F11E7',
    'file-edit-outline $F11E8', 'human-scooter $F11E9', 'spider $F11EA',
    'spider-thread $F11EB', 'plus-thick $F11EC', 'alert-circle-check $F11ED',
    'alert-circle-check-outline $F11EE', 'state-machine $F11EF', 'usb-port $F11F0',
    'cloud-lock $F11F1', 'cloud-lock-outline $F11F2', 'robot-mower-outline $F11F3',
    'share-all $F11F4', 'share-all-outline $F11F5', 'google-cloud $F11F6',
    'robot-mower $F11F7', 'fast-forward-5 $F11F8', 'rewind-5 $F11F9',
    'shape-oval-plus $F11FA', 'timeline-clock $F11FB', 'timeline-clock-outline $F11FC',
    'mirror $F11FD', 'account-multiple-check-outline $F11FE', 'card-plus $F11FF',
    'card-plus-outline $F1200', 'checkerboard-plus $F1201', 'checkerboard-minus $F1202',
    'checkerboard-remove $F1203', 'select-search $F1204', 'selection-search $F1205',
    'layers-search $F1206', 'layers-search-outline $F1207', 'lightbulb-cfl $F1208',
    'lightbulb-cfl-off $F1209', 'account-multiple-remove $F120A', 'account-multiple-remove-outline $F120B',
    'magnify-remove-cursor $F120C', 'magnify-remove-outline $F120D', 'archive-outline $F120E',
    'battery-heart $F120F', 'battery-heart-outline $F1210', 'battery-heart-variant $F1211',
    'bus-marker $F1212', 'chart-multiple $F1213', 'emoticon-lol $F1214',
    'emoticon-lol-outline $F1215', 'file-sync $F1216', 'file-sync-outline $F1217',
    'handshake $F1218', 'language-kotlin $F1219', 'language-fortran $F121A',
    'offer $F121B', 'radio-off $F121C', 'table-headers-eye $F121D',
    'table-headers-eye-off $F121E', 'tag-minus-outline $F121F', 'tag-off $F1220',
    'tag-off-outline $F1221', 'tag-plus-outline $F1222', 'tag-remove-outline $F1223',
    'tag-text $F1224', 'vector-polyline-edit $F1225', 'vector-polyline-minus $F1226',
    'vector-polyline-plus $F1227', 'vector-polyline-remove $F1228', 'beaker-alert $F1229',
    'beaker-alert-outline $F122A', 'beaker-check $F122B', 'beaker-check-outline $F122C',
    'beaker-minus $F122D', 'beaker-minus-outline $F122E', 'beaker-plus $F122F',
    'beaker-plus-outline $F1230', 'beaker-question $F1231', 'beaker-question-outline $F1232',
    'beaker-remove $F1233', 'beaker-remove-outline $F1234', 'bicycle-basket $F1235',
    'barcode-off $F1236', 'digital-ocean $F1237', 'exclamation-thick $F1238',
    'desk $F1239', 'flask-empty-minus $F123A', 'flask-empty-minus-outline $F123B',
    'flask-empty-plus $F123C', 'flask-empty-plus-outline $F123D', 'flask-empty-remove $F123E',
    'flask-empty-remove-outline $F123F', 'flask-minus $F1240', 'flask-minus-outline $F1241',
    'flask-plus $F1242', 'flask-plus-outline $F1243', 'flask-remove $F1244',
    'flask-remove-outline $F1245', 'folder-move-outline $F1246', 'home-remove $F1247',
    'webrtc $F1248', 'seat-passenger $F1249', 'web-clock $F124A',
    'flask-round-bottom $F124B', 'flask-round-bottom-empty $F124C', 'flask-round-bottom-empty-outline $F124D',
    'flask-round-bottom-outline $F124E', 'gold $F124F', 'message-star-outline $F1250',
    'home-lightbulb $F1251', 'home-lightbulb-outline $F1252', 'lightbulb-group $F1253',
    'lightbulb-group-outline $F1254', 'lightbulb-multiple $F1255', 'lightbulb-multiple-outline $F1256',
    'api-off $F1257', 'allergy $F1258', 'archive-arrow-down $F1259',
    'archive-arrow-down-outline $F125A', 'archive-arrow-up $F125B', 'archive-arrow-up-outline $F125C',
    'battery-off $F125D', 'battery-off-outline $F125E', 'bookshelf $F125F',
    'cash-minus $F1260', 'cash-plus $F1261', 'cash-remove $F1262',
    'clipboard-check-multiple $F1263', 'clipboard-check-multiple-outline $F1264', 'clipboard-file $F1265',
    'clipboard-file-outline $F1266', 'clipboard-multiple $F1267', 'clipboard-multiple-outline $F1268',
    'clipboard-play-multiple $F1269', 'clipboard-play-multiple-outline $F126A', 'clipboard-text-multiple $F126B',
    'clipboard-text-multiple-outline $F126C', 'folder-marker $F126D', 'folder-marker-outline $F126E',
    'format-list-text $F126F', 'inbox-arrow-down-outline $F1270', 'inbox-arrow-up-outline $F1271',
    'inbox-full $F1272', 'inbox-full-outline $F1273', 'inbox-outline $F1274',
    'lightbulb-cfl-spiral $F1275', 'magnify-scan $F1276', 'map-marker-multiple-outline $F1277',
    'percent-outline $F1278', 'phone-classic-off $F1279', 'play-box $F127A',
    'account-eye-outline $F127B', 'safe-square $F127C', 'safe-square-outline $F127D',
    'scoreboard $F127E', 'scoreboard-outline $F127F', 'select-marker $F1280',
    'select-multiple $F1281', 'select-multiple-marker $F1282', 'selection-marker $F1283',
    'selection-multiple-marker $F1284', 'selection-multiple $F1285', 'star-box-multiple $F1286',
    'star-box-multiple-outline $F1287', 'toy-brick $F1288', 'toy-brick-marker $F1289',
    'toy-brick-marker-outline $F128A', 'toy-brick-minus $F128B', 'toy-brick-minus-outline $F128C',
    'toy-brick-outline $F128D', 'toy-brick-plus $F128E', 'toy-brick-plus-outline $F128F',
    'toy-brick-remove $F1290', 'toy-brick-remove-outline $F1291', 'toy-brick-search $F1292',
    'toy-brick-search-outline $F1293', 'tray $F1294', 'tray-alert $F1295',
    'tray-full $F1296', 'tray-minus $F1297', 'tray-plus $F1298',
    'tray-remove $F1299', 'truck-check-outline $F129A', 'truck-delivery-outline $F129B',
    'truck-fast-outline $F129C', 'truck-outline $F129D', 'usb-flash-drive $F129E',
    'usb-flash-drive-outline $F129F', 'water-polo $F12A0', 'battery-low $F12A1',
    'battery-medium $F12A2', 'battery-high $F12A3', 'battery-charging-low $F12A4',
    'battery-charging-medium $F12A5', 'battery-charging-high $F12A6', 'hexadecimal $F12A7',
    'gesture-tap-button $F12A8', 'gesture-tap-box $F12A9', 'lan-check $F12AA',
    'keyboard-f1 $F12AB', 'keyboard-f2 $F12AC', 'keyboard-f3 $F12AD',
    'keyboard-f4 $F12AE', 'keyboard-f5 $F12AF', 'keyboard-f6 $F12B0',
    'keyboard-f7 $F12B1', 'keyboard-f8 $F12B2', 'keyboard-f9 $F12B3',
    'keyboard-f10 $F12B4', 'keyboard-f11 $F12B5', 'keyboard-f12 $F12B6',
    'keyboard-esc $F12B7', 'toslink $F12B8', 'cheese $F12B9',
    'string-lights $F12BA', 'string-lights-off $F12BB', 'whistle-outline $F12BC',
    'stairs-up $F12BD', 'stairs-down $F12BE', 'escalator-up $F12BF',
    'escalator-down $F12C0', 'elevator-up $F12C1', 'elevator-down $F12C2',
    'lightbulb-cfl-spiral-off $F12C3', 'comment-edit-outline $F12C4', 'tooltip-edit-outline $F12C5',
    'monitor-edit $F12C6', 'email-sync $F12C7', 'email-sync-outline $F12C8',
    'chat-alert-outline $F12C9', 'chat-processing-outline $F12CA', 'snowflake-melt $F12CB',
    'cloud-check-outline $F12CC', 'lightbulb-group-off $F12CD', 'lightbulb-group-off-outline $F12CE',
    'lightbulb-multiple-off $F12CF', 'lightbulb-multiple-off-outline $F12D0', 'chat-sleep $F12D1',
    'chat-sleep-outline $F12D2', 'garage-variant $F12D3', 'garage-open-variant $F12D4',
    'garage-alert-variant $F12D5', 'cloud-sync-outline $F12D6', 'globe-light $F12D7',
    'cellphone-nfc-off $F12D8', 'leaf-off $F12D9', 'leaf-maple-off $F12DA',
    'map-marker-left $F12DB', 'map-marker-right $F12DC', 'map-marker-left-outline $F12DD',
    'map-marker-right-outline $F12DE', 'account-cancel $F12DF', 'account-cancel-outline $F12E0',
    'file-clock $F12E1', 'file-clock-outline $F12E2', 'folder-table $F12E3',
    'folder-table-outline $F12E4', 'hydro-power $F12E5', 'doorbell $F12E6',
    'bulma $F12E7', 'iobroker $F12E8', 'oci $F12E9',
    'label-percent $F12EA', 'label-percent-outline $F12EB', 'checkbox-blank-off $F12EC',
    'checkbox-blank-off-outline $F12ED', 'square-off $F12EE', 'square-off-outline $F12EF',
    'drag-horizontal-variant $F12F0', 'drag-vertical-variant $F12F1', 'message-arrow-left $F12F2',
    'message-arrow-left-outline $F12F3', 'message-arrow-right $F12F4', 'message-arrow-right-outline $F12F5',
    'database-marker $F12F6', 'tag-multiple-outline $F12F7', 'map-marker-plus-outline $F12F8',
    'map-marker-minus-outline $F12F9', 'map-marker-remove-outline $F12FA', 'map-marker-check-outline $F12FB',
    'map-marker-radius-outline $F12FC', 'map-marker-off-outline $F12FD', 'molecule-co $F12FE',
    'jump-rope $F12FF', 'kettlebell $F1300', 'account-convert-outline $F1301',
    'bunk-bed $F1302', 'fleur-de-lis $F1303', 'ski $F1304',
    'ski-cross-country $F1305', 'ski-water $F1306', 'snowboard $F1307',
    'account-tie-voice $F1308', 'account-tie-voice-outline $F1309', 'account-tie-voice-off $F130A',
    'account-tie-voice-off-outline $F130B', 'beer-outline $F130C', 'glass-pint-outline $F130D',
    'coffee-to-go-outline $F130E', 'cup-outline $F130F', 'bottle-wine-outline $F1310',
    'earth-arrow-right $F1311', 'key-arrow-right $F1312', 'format-color-marker-cancel $F1313',
    'mother-heart $F1314', 'currency-eur-off $F1315', 'semantic-web $F1316',
    'kettle-alert $F1317', 'kettle-alert-outline $F1318', 'kettle-steam $F1319',
    'kettle-steam-outline $F131A', 'kettle-off $F131B', 'kettle-off-outline $F131C',
    'simple-icons $F131D', 'briefcase-check-outline $F131E', 'clipboard-plus-outline $F131F',
    'download-lock $F1320', 'download-lock-outline $F1321', 'hammer-screwdriver $F1322',
    'hammer-wrench $F1323', 'hydraulic-oil-level $F1324', 'hydraulic-oil-temperature $F1325',
    'medal-outline $F1326', 'rodent $F1327', 'abjad-arabic $F1328',
    'abjad-hebrew $F1329', 'abugida-devanagari $F132A', 'abugida-thai $F132B',
    'alphabet-aurebesh $F132C', 'alphabet-cyrillic $F132D', 'alphabet-greek $F132E',
    'alphabet-latin $F132F', 'alphabet-piqad $F1330', 'ideogram-cjk $F1331',
    'ideogram-cjk-variant $F1332', 'syllabary-hangul $F1333', 'syllabary-hiragana $F1334',
    'syllabary-katakana $F1335', 'syllabary-katakana-halfwidth $F1336', 'alphabet-tengwar $F1337',
    'head-alert $F1338', 'head-alert-outline $F1339', 'head-check $F133A',
    'head-check-outline $F133B', 'head-cog $F133C', 'head-cog-outline $F133D',
    'head-dots-horizontal $F133E', 'head-dots-horizontal-outline $F133F', 'head-flash $F1340',
    'head-flash-outline $F1341', 'head-heart $F1342', 'head-heart-outline $F1343',
    'head-lightbulb $F1344', 'head-lightbulb-outline $F1345', 'head-minus $F1346',
    'head-minus-outline $F1347', 'head-plus $F1348', 'head-plus-outline $F1349',
    'head-question $F134A', 'head-question-outline $F134B', 'head-remove $F134C',
    'head-remove-outline $F134D', 'head-snowflake $F134E', 'head-snowflake-outline $F134F',
    'head-sync $F1350', 'head-sync-outline $F1351', 'hvac $F1352',
    'pencil-ruler $F1353', 'pipe-wrench $F1354', 'widgets-outline $F1355',
    'television-ambient-light $F1356', 'propane-tank $F1357', 'propane-tank-outline $F1358',
    'folder-music $F1359', 'folder-music-outline $F135A', 'klingon $F135B',
    'palette-swatch-outline $F135C', 'form-textbox-lock $F135D', 'head $F135E',
    'head-outline $F135F', 'shield-half $F1360', 'store-outline $F1361',
    'google-downasaur $F1362', 'bottle-soda-classic-outline $F1363', 'sticker $F1364',
    'sticker-alert $F1365', 'sticker-alert-outline $F1366', 'sticker-check $F1367',
    'sticker-check-outline $F1368', 'sticker-minus $F1369', 'sticker-minus-outline $F136A',
    'sticker-outline $F136B', 'sticker-plus $F136C', 'sticker-plus-outline $F136D',
    'sticker-remove $F136E', 'sticker-remove-outline $F136F', 'account-cog $F1370',
    'account-cog-outline $F1371', 'account-details-outline $F1372', 'upload-lock $F1373',
    'upload-lock-outline $F1374', 'label-multiple $F1375', 'label-multiple-outline $F1376',
    'refresh-circle $F1377', 'sync-circle $F1378', 'bookmark-music-outline $F1379',
    'bookmark-remove-outline $F137A', 'bookmark-check-outline $F137B', 'traffic-cone $F137C',
    'cup-off-outline $F137D', 'auto-download $F137E', 'shuriken $F137F',
    'chart-ppf $F1380', 'elevator-passenger $F1381', 'compass-rose $F1382',
    'space-station $F1383', 'order-bool-descending $F1384', 'sort-bool-ascending $F1385',
    'sort-bool-ascending-variant $F1386', 'sort-bool-descending $F1387', 'sort-bool-descending-variant $F1388',
    'sort-numeric-ascending $F1389', 'sort-numeric-descending $F138A', 'human-baby-changing-table $F138B',
    'human-male-child $F138C', 'human-wheelchair $F138D', 'microsoft-access $F138E',
    'microsoft-excel $F138F', 'microsoft-powerpoint $F1390', 'microsoft-sharepoint $F1391',
    'microsoft-word $F1392', 'nintendo-game-boy $F1393', 'cable-data $F1394',
    'circle-half $F1395', 'circle-half-full $F1396', 'cellphone-charging $F1397',
    'close-thick $F1398', 'escalator-box $F1399', 'lock-check $F139A',
    'lock-open-alert $F139B', 'lock-open-check $F139C', 'recycle-variant $F139D',
    'stairs-box $F139E', 'hand-water $F139F', 'table-refresh $F13A0',
    'table-sync $F13A1', 'size-xxs $F13A2', 'size-xs $F13A3',
    'size-s $F13A4', 'size-m $F13A5', 'size-xl $F13A7',
    'size-xxl $F13A8', 'size-xxxl $F13A9', 'ticket-confirmation-outline $F13AA',
    'timer $F13AB', 'timer-off $F13AC', 'book-account $F13AD',
    'book-account-outline $F13AE', 'rocket-outline $F13AF', 'home-search $F13B0',
    'home-search-outline $F13B1', 'car-arrow-left $F13B2', 'car-arrow-right $F13B3',
    'monitor-eye $F13B4', 'lipstick $F13B5', 'virus $F13B6',
    'virus-outline $F13B7', 'text-search $F13B8', 'table-account $F13B9',
    'table-alert $F13BA', 'table-arrow-down $F13BB', 'table-arrow-left $F13BC',
    'table-arrow-right $F13BD', 'table-arrow-up $F13BE', 'table-cancel $F13BF',
    'table-check $F13C0', 'table-clock $F13C1', 'table-cog $F13C2',
    'table-eye-off $F13C3', 'table-heart $F13C4', 'table-key $F13C5',
    'table-lock $F13C6', 'table-minus $F13C7', 'table-multiple $F13C8',
    'table-network $F13C9', 'table-off $F13CA', 'table-star $F13CB',
    'car-cog $F13CC', 'car-settings $F13CD', 'cog-off $F13CE',
    'cog-off-outline $F13CF', 'credit-card-check $F13D0', 'credit-card-check-outline $F13D1',
    'file-tree-outline $F13D2', 'folder-star-multiple $F13D3', 'folder-star-multiple-outline $F13D4',
    'home-minus-outline $F13D5', 'home-plus-outline $F13D6', 'home-remove-outline $F13D7',
    'scan-helper $F13D8', 'video-3d-off $F13D9', 'shield-bug $F13DA',
    'shield-bug-outline $F13DB', 'eyedropper-plus $F13DC', 'eyedropper-minus $F13DD',
    'eyedropper-remove $F13DE', 'eyedropper-off $F13DF', 'baby-buggy $F13E0',
    'umbrella-closed-variant $F13E1', 'umbrella-closed-outline $F13E2', 'email-off $F13E3',
    'email-off-outline $F13E4', 'food-variant-off $F13E5', 'play-box-multiple-outline $F13E6',
    'bell-cancel $F13E7', 'bell-cancel-outline $F13E8', 'bell-minus $F13E9',
    'bell-minus-outline $F13EA', 'bell-remove $F13EB', 'bell-remove-outline $F13EC',
    'beehive-off-outline $F13ED', 'cheese-off $F13EE', 'corn-off $F13EF',
    'egg-off $F13F0', 'egg-off-outline $F13F1', 'egg-outline $F13F2',
    'fish-off $F13F3', 'flask-empty-off $F13F4', 'flask-empty-off-outline $F13F5',
    'flask-off $F13F6', 'flask-off-outline $F13F7', 'fruit-cherries-off $F13F8',
    'fruit-citrus-off $F13F9', 'mushroom-off $F13FA', 'mushroom-off-outline $F13FB',
    'soy-sauce-off $F13FC', 'seed-off $F13FD', 'seed-off-outline $F13FE',
    'tailwind $F13FF', 'form-dropdown $F1400', 'form-select $F1401',
    'pump $F1402', 'earth-plus $F1403', 'earth-minus $F1404',
    'earth-remove $F1405', 'earth-box-plus $F1406', 'earth-box-minus $F1407',
    'earth-box-remove $F1408', 'gas-station-off $F1409', 'gas-station-off-outline $F140A',
    'lightning-bolt $F140B', 'lightning-bolt-outline $F140C', 'smoking-pipe $F140D',
    'axis-arrow-info $F140E', 'chat-plus $F140F', 'chat-minus $F1410',
    'chat-remove $F1411', 'chat-plus-outline $F1412', 'chat-minus-outline $F1413',
    'chat-remove-outline $F1414', 'bucket $F1415', 'bucket-outline $F1416',
    'pail $F1417', 'image-remove $F1418', 'image-minus $F1419',
    'pine-tree-fire $F141A', 'cigar-off $F141B', 'cube-off $F141C',
    'cube-off-outline $F141D', 'dome-light $F141E', 'food-drumstick $F141F',
    'food-drumstick-outline $F1420', 'incognito-circle $F1421', 'incognito-circle-off $F1422',
    'microwave-off $F1423', 'power-plug-off-outline $F1424', 'power-plug-outline $F1425',
    'puzzle-check $F1426', 'puzzle-check-outline $F1427', 'smoking-pipe-off $F1428',
    'spoon-sugar $F1429', 'table-split-cell $F142A', 'ticket-percent-outline $F142B',
    'fuse-off $F142C', 'fuse-alert $F142D', 'heart-plus $F142E',
    'heart-minus $F142F', 'heart-remove $F1430', 'heart-plus-outline $F1431',
    'heart-minus-outline $F1432', 'heart-remove-outline $F1433', 'heart-off-outline $F1434',
    'motion-sensor-off $F1435', 'pail-plus $F1436', 'pail-minus $F1437',
    'pail-remove $F1438', 'pail-off $F1439', 'pail-outline $F143A',
    'pail-plus-outline $F143B', 'pail-minus-outline $F143C', 'pail-remove-outline $F143D',
    'pail-off-outline $F143E', 'clock-time-one $F143F', 'clock-time-two $F1440',
    'clock-time-three $F1441', 'clock-time-four $F1442', 'clock-time-five $F1443',
    'clock-time-six $F1444', 'clock-time-seven $F1445', 'clock-time-eight $F1446',
    'clock-time-nine $F1447', 'clock-time-ten $F1448', 'clock-time-eleven $F1449',
    'clock-time-twelve $F144A', 'clock-time-one-outline $F144B', 'clock-time-two-outline $F144C',
    'clock-time-three-outline $F144D', 'clock-time-four-outline $F144E', 'clock-time-five-outline $F144F',
    'clock-time-six-outline $F1450', 'clock-time-seven-outline $F1451', 'clock-time-eight-outline $F1452',
    'clock-time-nine-outline $F1453', 'clock-time-ten-outline $F1454', 'clock-time-eleven-outline $F1455',
    'clock-time-twelve-outline $F1456', 'printer-search $F1457', 'printer-eye $F1458',
    'minus-circle-off $F1459', 'minus-circle-off-outline $F145A', 'content-save-cog $F145B',
    'content-save-cog-outline $F145C', 'set-square $F145D', 'cog-refresh $F145E',
    'cog-refresh-outline $F145F', 'cog-sync $F1460', 'cog-sync-outline $F1461',
    'download-box $F1462', 'download-box-outline $F1463', 'download-circle $F1464',
    'download-circle-outline $F1465', 'air-humidifier-off $F1466', 'chili-off $F1467',
    'food-drumstick-off $F1468', 'food-drumstick-off-outline $F1469', 'food-steak $F146A',
    'food-steak-off $F146B', 'fan-alert $F146C', 'fan-chevron-down $F146D',
    'fan-chevron-up $F146E', 'fan-plus $F146F', 'fan-minus $F1470',
    'fan-remove $F1471', 'fan-speed-1 $F1472', 'fan-speed-2 $F1473',
    'fan-speed-3 $F1474', 'rug $F1475', 'lingerie $F1476',
    'wizard-hat $F1477', 'hours-24 $F1478', 'cosine-wave $F1479',
    'sawtooth-wave $F147A', 'square-wave $F147B', 'triangle-wave $F147C',
    'waveform $F147D', 'folder-multiple-plus $F147E', 'folder-multiple-plus-outline $F147F',
    'current-ac $F1480', 'watering-can $F1481', 'watering-can-outline $F1482',
    'monitor-share $F1483', 'laser-pointer $F1484', 'view-array-outline $F1485',
    'view-carousel-outline $F1486', 'view-column-outline $F1487', 'view-comfy-outline $F1488',
    'view-dashboard-variant-outline $F1489', 'view-day-outline $F148A', 'view-list-outline $F148B',
    'view-module-outline $F148C', 'view-parallel-outline $F148D', 'view-quilt-outline $F148E',
    'view-sequential-outline $F148F', 'view-stream-outline $F1490', 'view-week-outline $F1491',
    'compare-horizontal $F1492', 'compare-vertical $F1493', 'briefcase-variant $F1494',
    'briefcase-variant-outline $F1495', 'relation-many-to-many $F1496', 'relation-many-to-one $F1497',
    'relation-many-to-one-or-many $F1498', 'relation-many-to-only-one $F1499', 'relation-many-to-zero-or-many $F149A',
    'relation-many-to-zero-or-one $F149B', 'relation-one-or-many-to-many $F149C', 'relation-one-or-many-to-one $F149D',
    'relation-one-or-many-to-one-or-many $F149E', 'relation-one-or-many-to-only-one $F149F', 'relation-one-or-many-to-zero-or-many $F14A0',
    'relation-one-or-many-to-zero-or-one $F14A1', 'relation-one-to-many $F14A2', 'relation-one-to-one $F14A3',
    'relation-one-to-one-or-many $F14A4', 'relation-one-to-only-one $F14A5', 'relation-one-to-zero-or-many $F14A6',
    'relation-one-to-zero-or-one $F14A7', 'relation-only-one-to-many $F14A8', 'relation-only-one-to-one $F14A9',
    'relation-only-one-to-one-or-many $F14AA', 'relation-only-one-to-only-one $F14AB', 'relation-only-one-to-zero-or-many $F14AC',
    'relation-only-one-to-zero-or-one $F14AD', 'relation-zero-or-many-to-many $F14AE', 'relation-zero-or-many-to-one $F14AF',
    'relation-zero-or-many-to-one-or-many $F14B0', 'relation-zero-or-many-to-only-one $F14B1', 'relation-zero-or-many-to-zero-or-many $F14B2',
    'relation-zero-or-many-to-zero-or-one $F14B3', 'relation-zero-or-one-to-many $F14B4', 'relation-zero-or-one-to-one $F14B5',
    'relation-zero-or-one-to-one-or-many $F14B6', 'relation-zero-or-one-to-only-one $F14B7', 'relation-zero-or-one-to-zero-or-many $F14B8',
    'relation-zero-or-one-to-zero-or-one $F14B9', 'alert-plus $F14BA', 'alert-minus $F14BB',
    'alert-remove $F14BC', 'alert-plus-outline $F14BD', 'alert-minus-outline $F14BE',
    'alert-remove-outline $F14BF', 'carabiner $F14C0', 'fencing $F14C1',
    'skateboard $F14C2', 'polo $F14C3', 'tractor-variant $F14C4',
    'radiology-box $F14C5', 'radiology-box-outline $F14C6', 'skull-scan $F14C7',
    'skull-scan-outline $F14C8', 'plus-minus-variant $F14C9', 'source-branch-plus $F14CA',
    'source-branch-minus $F14CB', 'source-branch-remove $F14CC', 'source-branch-refresh $F14CD',
    'source-branch-sync $F14CE', 'source-branch-check $F14CF', 'puzzle-plus $F14D0',
    'puzzle-minus $F14D1', 'puzzle-remove $F14D2', 'puzzle-edit $F14D3',
    'puzzle-heart $F14D4', 'puzzle-star $F14D5', 'puzzle-plus-outline $F14D6',
    'puzzle-minus-outline $F14D7', 'puzzle-remove-outline $F14D8', 'puzzle-edit-outline $F14D9',
    'puzzle-heart-outline $F14DA', 'puzzle-star-outline $F14DB', 'rhombus-medium-outline $F14DC',
    'rhombus-split-outline $F14DD', 'rocket-launch $F14DE', 'rocket-launch-outline $F14DF',
    'set-merge $F14E0', 'set-split $F14E1', 'beekeeper $F14E2',
    'snowflake-off $F14E3', 'weather-sunny-off $F14E4', 'clipboard-edit $F14E5',
    'clipboard-edit-outline $F14E6', 'notebook-edit $F14E7', 'human-edit $F14E8',
    'notebook-edit-outline $F14E9', 'cash-lock $F14EA', 'cash-lock-open $F14EB',
    'account-supervisor-circle-outline $F14EC', 'car-outline $F14ED', 'cash-check $F14EE',
    'filter-off $F14EF', 'filter-off-outline $F14F0', 'spirit-level $F14F1',
    'wheel-barrow $F14F2', 'book-check $F14F3', 'book-check-outline $F14F4',
    'notebook-check $F14F5', 'notebook-check-outline $F14F6', 'book-open-variant $F14F7',
    'sign-pole $F14F8', 'shore $F14F9', 'shape-square-rounded-plus $F14FA',
    'square-rounded $F14FB', 'square-rounded-outline $F14FC', 'archive-alert $F14FD',
    'archive-alert-outline $F14FE', 'power-socket-it $F14FF', 'square-circle $F1500',
    'symbol $F1501', 'water-alert $F1502', 'water-alert-outline $F1503',
    'water-check $F1504', 'water-check-outline $F1505', 'water-minus $F1506',
    'water-minus-outline $F1507', 'water-off-outline $F1508', 'water-percent-alert $F1509',
    'water-plus $F150A', 'water-plus-outline $F150B', 'water-remove $F150C',
    'water-remove-outline $F150D', 'snake $F150E', 'format-text-variant-outline $F150F',
    'grass $F1510', 'access-point-off $F1511', 'currency-mnt $F1512',
    'dock-top $F1513', 'share-variant-outline $F1514', 'transit-skip $F1515',
    'yurt $F1516', 'file-document-multiple $F1517', 'file-document-multiple-outline $F1518',
    'ev-plug-ccs1 $F1519', 'ev-plug-ccs2 $F151A', 'ev-plug-chademo $F151B',
    'ev-plug-tesla $F151C', 'ev-plug-type1 $F151D', 'ev-plug-type2 $F151E',
    'office-building-outline $F151F', 'office-building-marker $F1520', 'office-building-marker-outline $F1521',
    'progress-question $F1522', 'basket-minus $F1523', 'basket-minus-outline $F1524',
    'basket-off $F1525', 'basket-off-outline $F1526', 'basket-plus $F1527',
    'basket-plus-outline $F1528', 'basket-remove $F1529', 'basket-remove-outline $F152A',
    'account-reactivate $F152B', 'account-reactivate-outline $F152C', 'car-lifted-pickup $F152D',
    'video-high-definition $F152E', 'phone-remove $F152F', 'phone-remove-outline $F1530',
    'thermometer-off $F1531', 'timeline-check $F1532', 'timeline-check-outline $F1533',
    'timeline-minus $F1534', 'timeline-minus-outline $F1535', 'timeline-remove $F1536',
    'timeline-remove-outline $F1537', 'access-point-check $F1538', 'access-point-minus $F1539',
    'access-point-plus $F153A', 'access-point-remove $F153B', 'data-matrix $F153C',
    'data-matrix-edit $F153D', 'data-matrix-minus $F153E', 'data-matrix-plus $F153F',
    'data-matrix-remove $F1540', 'data-matrix-scan $F1541', 'tune-variant $F1542',
    'tune-vertical-variant $F1543', 'rake $F1544', 'shimmer $F1545',
    'transit-connection-horizontal $F1546', 'sort-calendar-ascending $F1547', 'sort-calendar-descending $F1548',
    'sort-clock-ascending $F1549', 'sort-clock-ascending-outline $F154A', 'sort-clock-descending $F154B',
    'sort-clock-descending-outline $F154C', 'chart-box $F154D', 'chart-box-outline $F154E',
    'chart-box-plus-outline $F154F', 'mouse-move-down $F1550', 'mouse-move-up $F1551',
    'mouse-move-vertical $F1552', 'pitchfork $F1553', 'vanish-quarter $F1554',
    'application-settings-outline $F1555', 'delete-clock $F1556', 'delete-clock-outline $F1557',
    'kangaroo $F1558', 'phone-dial $F1559', 'phone-dial-outline $F155A',
    'star-off-outline $F155B', 'tooltip-check $F155C', 'tooltip-check-outline $F155D',
    'tooltip-minus $F155E', 'tooltip-minus-outline $F155F', 'tooltip-remove $F1560',
    'tooltip-remove-outline $F1561', 'pretzel $F1562', 'star-plus $F1563',
    'star-minus $F1564', 'star-remove $F1565', 'star-check $F1566',
    'star-plus-outline $F1567', 'star-minus-outline $F1568', 'star-remove-outline $F1569',
    'star-check-outline $F156A', 'eiffel-tower $F156B', 'submarine $F156C',
    'sofa-outline $F156D', 'sofa-single $F156E', 'sofa-single-outline $F156F',
    'text-account $F1570', 'human-queue $F1571', 'food-halal $F1572',
    'food-kosher $F1573', 'key-chain $F1574', 'key-chain-variant $F1575',
    'lamps $F1576', 'application-cog-outline $F1577', 'dance-pole $F1578',
    'social-distance-2-meters $F1579', 'social-distance-6-feet $F157A', 'calendar-cursor $F157B',
    'emoticon-sick $F157C', 'emoticon-sick-outline $F157D', 'hand-heart-outline $F157E',
    'hand-wash $F157F', 'hand-wash-outline $F1580', 'human-cane $F1581',
    'lotion $F1582', 'lotion-outline $F1583', 'lotion-plus $F1584',
    'lotion-plus-outline $F1585', 'face-mask $F1586', 'face-mask-outline $F1587',
    'reiterate $F1588', 'butterfly $F1589', 'butterfly-outline $F158A',
    'bag-suitcase $F158B', 'bag-suitcase-outline $F158C', 'bag-suitcase-off $F158D',
    'bag-suitcase-off-outline $F158E', 'motion-play $F158F', 'motion-pause $F1590',
    'motion-play-outline $F1591', 'motion-pause-outline $F1592', 'arrow-top-left-thin-circle-outline $F1593',
    'arrow-top-right-thin-circle-outline $F1594', 'arrow-bottom-right-thin-circle-outline $F1595', 'arrow-bottom-left-thin-circle-outline $F1596',
    'arrow-up-thin-circle-outline $F1597', 'arrow-right-thin-circle-outline $F1598', 'arrow-down-thin-circle-outline $F1599',
    'arrow-left-thin-circle-outline $F159A', 'human-capacity-decrease $F159B', 'human-capacity-increase $F159C',
    'human-greeting-proximity $F159D', 'hvac-off $F159E', 'inbox-remove $F159F',
    'inbox-remove-outline $F15A0', 'handshake-outline $F15A1', 'ladder $F15A2',
    'router-wireless-off $F15A3', 'seesaw $F15A4', 'slide $F15A5',
    'calculator-variant-outline $F15A6', 'shield-account-variant $F15A7', 'shield-account-variant-outline $F15A8',
    'message-flash $F15A9', 'message-flash-outline $F15AA', 'list-status $F15AB',
    'message-bookmark $F15AC', 'message-bookmark-outline $F15AD', 'comment-bookmark $F15AE',
    'comment-bookmark-outline $F15AF', 'comment-flash $F15B0', 'comment-flash-outline $F15B1',
    'motion $F15B2', 'motion-outline $F15B3', 'bicycle-electric $F15B4',
    'car-electric-outline $F15B5', 'chart-timeline-variant-shimmer $F15B6', 'moped-electric $F15B7',
    'moped-electric-outline $F15B8', 'moped-outline $F15B9', 'motorbike-electric $F15BA',
    'rickshaw $F15BB', 'rickshaw-electric $F15BC', 'scooter $F15BD',
    'scooter-electric $F15BE', 'horse $F15BF', 'horse-human $F15C0',
    'horse-variant $F15C1', 'unicorn $F15C2', 'unicorn-variant $F15C3',
    'alarm-panel $F15C4', 'alarm-panel-outline $F15C5', 'bird $F15C6',
    'shoe-cleat $F15C7', 'shoe-sneaker $F15C8', 'human-female-dance $F15C9',
    'shoe-ballet $F15CA', 'numeric-positive-1 $F15CB', 'face-man-shimmer $F15CC',
    'face-man-shimmer-outline $F15CD', 'face-woman-shimmer $F15CE', 'face-woman-shimmer-outline $F15CF',
    'home-alert-outline $F15D0', 'lock-alert-outline $F15D1', 'lock-open-alert-outline $F15D2',
    'sim-alert-outline $F15D3', 'sim-off-outline $F15D4', 'sim-outline $F15D5',
    'book-open-page-variant-outline $F15D6', 'fire-alert $F15D7', 'ray-start-vertex-end $F15D8',
    'camera-flip $F15D9', 'camera-flip-outline $F15DA', 'orbit-variant $F15DB',
    'circle-box $F15DC', 'circle-box-outline $F15DD', 'mustache $F15DE',
    'comment-minus $F15DF', 'comment-minus-outline $F15E0', 'comment-off $F15E1',
    'comment-off-outline $F15E2', 'eye-remove $F15E3', 'eye-remove-outline $F15E4',
    'unicycle $F15E5', 'glass-cocktail-off $F15E6', 'glass-mug-off $F15E7',
    'glass-mug-variant-off $F15E8', 'bicycle-penny-farthing $F15E9', 'cart-check $F15EA',
    'cart-variant $F15EB', 'baseball-diamond $F15EC', 'baseball-diamond-outline $F15ED',
    'fridge-industrial $F15EE', 'fridge-industrial-alert $F15EF', 'fridge-industrial-alert-outline $F15F0',
    'fridge-industrial-off $F15F1', 'fridge-industrial-off-outline $F15F2', 'fridge-industrial-outline $F15F3',
    'fridge-variant $F15F4', 'fridge-variant-alert $F15F5', 'fridge-variant-alert-outline $F15F6',
    'fridge-variant-off $F15F7', 'fridge-variant-off-outline $F15F8', 'fridge-variant-outline $F15F9',
    'windsock $F15FA', 'dance-ballroom $F15FB', 'dots-grid $F15FC',
    'dots-square $F15FD', 'dots-triangle $F15FE', 'dots-hexagon $F15FF',
    'card-minus $F1600', 'card-minus-outline $F1601', 'card-off $F1602',
    'card-off-outline $F1603', 'card-remove $F1604', 'card-remove-outline $F1605',
    'torch $F1606', 'navigation-outline $F1607', 'map-marker-star $F1608',
    'map-marker-star-outline $F1609', 'manjaro $F160A', 'fast-forward-60 $F160B',
    'rewind-60 $F160C', 'image-text $F160D', 'family-tree $F160E',
    'car-emergency $F160F', 'notebook-minus $F1610', 'notebook-minus-outline $F1611',
    'notebook-plus $F1612', 'notebook-plus-outline $F1613', 'notebook-remove $F1614',
    'notebook-remove-outline $F1615', 'connection $F1616', 'language-rust $F1617',
    'clipboard-minus $F1618', 'clipboard-minus-outline $F1619', 'clipboard-off $F161A',
    'clipboard-off-outline $F161B', 'clipboard-remove $F161C', 'clipboard-remove-outline $F161D',
    'clipboard-search $F161E', 'clipboard-search-outline $F161F', 'clipboard-text-off $F1620',
    'clipboard-text-off-outline $F1621', 'clipboard-text-search $F1622', 'clipboard-text-search-outline $F1623',
    'database-alert-outline $F1624', 'database-arrow-down-outline $F1625', 'database-arrow-left-outline $F1626',
    'database-arrow-right-outline $F1627', 'database-arrow-up-outline $F1628', 'database-check-outline $F1629',
    'database-clock-outline $F162A', 'database-edit-outline $F162B', 'database-export-outline $F162C',
    'database-import-outline $F162D', 'database-lock-outline $F162E', 'database-marker-outline $F162F',
    'database-minus-outline $F1630', 'database-off-outline $F1631', 'database-outline $F1632',
    'database-plus-outline $F1633', 'database-refresh-outline $F1634', 'database-remove-outline $F1635',
    'database-search-outline $F1636', 'database-settings-outline $F1637', 'database-sync-outline $F1638',
    'minus-thick $F1639', 'database-alert $F163A', 'database-arrow-down $F163B',
    'database-arrow-left $F163C', 'database-arrow-right $F163D', 'database-arrow-up $F163E',
    'database-clock $F163F', 'database-off $F1640', 'calendar-lock $F1641',
    'calendar-lock-outline $F1642', 'content-save-off $F1643', 'content-save-off-outline $F1644',
    'credit-card-refresh $F1645', 'credit-card-refresh-outline $F1646', 'credit-card-search $F1647',
    'credit-card-search-outline $F1648', 'credit-card-sync $F1649', 'credit-card-sync-outline $F164A',
    'database-cog $F164B', 'database-cog-outline $F164C', 'message-off $F164D',
    'message-off-outline $F164E', 'note-minus $F164F', 'note-minus-outline $F1650',
    'note-remove $F1651', 'note-remove-outline $F1652', 'note-search $F1653',
    'note-search-outline $F1654', 'bank-check $F1655', 'bank-off $F1656',
    'bank-off-outline $F1657', 'briefcase-off $F1658', 'briefcase-off-outline $F1659',
    'briefcase-variant-off $F165A', 'briefcase-variant-off-outline $F165B', 'ghost-off-outline $F165C',
    'ghost-outline $F165D', 'store-minus $F165E', 'store-plus $F165F',
    'store-remove $F1660', 'email-remove $F1661', 'email-remove-outline $F1662',
    'heart-cog $F1663', 'heart-cog-outline $F1664', 'heart-settings $F1665',
    'heart-settings-outline $F1666', 'pentagram $F1667', 'star-cog $F1668',
    'star-cog-outline $F1669', 'star-settings $F166A', 'star-settings-outline $F166B',
    'calendar-end $F166C', 'calendar-start $F166D', 'cannabis-off $F166E',
    'mower $F166F', 'mower-bag $F1670', 'lock-off $F1671',
    'lock-off-outline $F1672', 'shark-fin $F1673', 'shark-fin-outline $F1674',
    'paw-outline $F1675', 'paw-off-outline $F1676', 'snail $F1677',
    'pig-variant-outline $F1678', 'piggy-bank-outline $F1679', 'robot-outline $F167A',
    'robot-off-outline $F167B', 'book-alert $F167C', 'book-alert-outline $F167D',
    'book-arrow-down $F167E', 'book-arrow-down-outline $F167F', 'book-arrow-left $F1680',
    'book-arrow-left-outline $F1681', 'book-arrow-right $F1682', 'book-arrow-right-outline $F1683',
    'book-arrow-up $F1684', 'book-arrow-up-outline $F1685', 'book-cancel $F1686',
    'book-cancel-outline $F1687', 'book-clock $F1688', 'book-clock-outline $F1689',
    'book-cog $F168A', 'book-cog-outline $F168B', 'book-edit $F168C',
    'book-edit-outline $F168D', 'book-lock-open-outline $F168E', 'book-lock-outline $F168F',
    'book-marker $F1690', 'book-marker-outline $F1691', 'book-minus-outline $F1692',
    'book-music-outline $F1693', 'book-off $F1694', 'book-off-outline $F1695',
    'book-plus-outline $F1696', 'book-refresh $F1697', 'book-refresh-outline $F1698',
    'book-remove-outline $F1699', 'book-settings $F169A', 'book-settings-outline $F169B',
    'book-sync $F169C', 'robot-angry $F169D', 'robot-angry-outline $F169E',
    'robot-confused $F169F', 'robot-confused-outline $F16A0', 'robot-dead $F16A1',
    'robot-dead-outline $F16A2', 'robot-excited $F16A3', 'robot-excited-outline $F16A4',
    'robot-love $F16A5', 'robot-love-outline $F16A6', 'robot-off $F16A7',
    'lock-check-outline $F16A8', 'lock-minus $F16A9', 'lock-minus-outline $F16AA',
    'lock-open-check-outline $F16AB', 'lock-open-minus $F16AC', 'lock-open-minus-outline $F16AD',
    'lock-open-plus $F16AE', 'lock-open-plus-outline $F16AF', 'lock-open-remove $F16B0',
    'lock-open-remove-outline $F16B1', 'lock-plus-outline $F16B2', 'lock-remove $F16B3',
    'lock-remove-outline $F16B4', 'wifi-alert $F16B5', 'wifi-arrow-down $F16B6',
    'wifi-arrow-left $F16B7', 'wifi-arrow-left-right $F16B8', 'wifi-arrow-right $F16B9',
    'wifi-arrow-up $F16BA', 'wifi-arrow-up-down $F16BB', 'wifi-cancel $F16BC',
    'wifi-check $F16BD', 'wifi-cog $F16BE', 'wifi-lock $F16BF',
    'wifi-lock-open $F16C0', 'wifi-marker $F16C1', 'wifi-minus $F16C2',
    'wifi-plus $F16C3', 'wifi-refresh $F16C4', 'wifi-remove $F16C5',
    'wifi-settings $F16C6', 'wifi-sync $F16C7', 'book-sync-outline $F16C8',
    'book-education $F16C9', 'book-education-outline $F16CA', 'wifi-strength-1-lock-open $F16CB',
    'wifi-strength-2-lock-open $F16CC', 'wifi-strength-3-lock-open $F16CD', 'wifi-strength-4-lock-open $F16CE',
    'wifi-strength-lock-open-outline $F16CF', 'cookie-alert $F16D0', 'cookie-alert-outline $F16D1',
    'cookie-check $F16D2', 'cookie-check-outline $F16D3', 'cookie-cog $F16D4',
    'cookie-cog-outline $F16D5', 'cookie-plus $F16D6', 'cookie-plus-outline $F16D7',
    'cookie-remove $F16D8', 'cookie-remove-outline $F16D9', 'cookie-minus $F16DA',
    'cookie-minus-outline $F16DB', 'cookie-settings $F16DC', 'cookie-settings-outline $F16DD',
    'cookie-outline $F16DE', 'tape-drive $F16DF', 'abacus $F16E0',
    'calendar-clock-outline $F16E1', 'clipboard-clock $F16E2', 'clipboard-clock-outline $F16E3',
    'cookie-clock $F16E4', 'cookie-clock-outline $F16E5', 'cookie-edit $F16E6',
    'cookie-edit-outline $F16E7', 'cookie-lock $F16E8', 'cookie-lock-outline $F16E9',
    'cookie-off $F16EA', 'cookie-off-outline $F16EB', 'cookie-refresh $F16EC',
    'cookie-refresh-outline $F16ED', 'dog-side-off $F16EE', 'gift-off $F16EF',
    'gift-off-outline $F16F0', 'gift-open $F16F1', 'gift-open-outline $F16F2',
    'movie-check $F16F3', 'movie-check-outline $F16F4', 'movie-cog $F16F5',
    'movie-cog-outline $F16F6', 'movie-minus $F16F7', 'movie-minus-outline $F16F8',
    'movie-off $F16F9', 'movie-off-outline $F16FA', 'movie-open-check $F16FB',
    'movie-open-check-outline $F16FC', 'movie-open-cog $F16FD', 'movie-open-cog-outline $F16FE',
    'movie-open-edit $F16FF', 'movie-open-edit-outline $F1700', 'movie-open-minus $F1701',
    'movie-open-minus-outline $F1702', 'movie-open-off $F1703', 'movie-open-off-outline $F1704',
    'movie-open-play $F1705', 'movie-open-play-outline $F1706', 'movie-open-plus $F1707',
    'movie-open-plus-outline $F1708', 'movie-open-remove $F1709', 'movie-open-remove-outline $F170A',
    'movie-open-settings $F170B', 'movie-open-settings-outline $F170C', 'movie-open-star $F170D',
    'movie-open-star-outline $F170E', 'movie-play $F170F', 'movie-play-outline $F1710',
    'movie-plus $F1711', 'movie-plus-outline $F1712', 'movie-remove $F1713',
    'movie-remove-outline $F1714', 'movie-settings $F1715', 'movie-settings-outline $F1716',
    'movie-star $F1717', 'movie-star-outline $F1718', 'robot-happy $F1719',
    'robot-happy-outline $F171A', 'turkey $F171B', 'food-turkey $F171C',
    'fan-auto $F171D', 'alarm-light-off $F171E', 'alarm-light-off-outline $F171F',
    'broadcast $F1720', 'broadcast-off $F1721', 'fire-off $F1722',
    'firework-off $F1723', 'projector-screen-outline $F1724', 'script-text-key $F1725',
    'script-text-key-outline $F1726', 'script-text-play $F1727', 'script-text-play-outline $F1728',
    'surround-sound-2-1 $F1729', 'surround-sound-5-1-2 $F172A', 'tag-arrow-down $F172B',
    'tag-arrow-down-outline $F172C', 'tag-arrow-left $F172D', 'tag-arrow-left-outline $F172E',
    'tag-arrow-right $F172F', 'tag-arrow-right-outline $F1730', 'tag-arrow-up $F1731',
    'tag-arrow-up-outline $F1732', 'train-car-passenger $F1733', 'train-car-passenger-door $F1734',
    'train-car-passenger-door-open $F1735', 'train-car-passenger-variant $F1736', 'webcam-off $F1737',
    'chat-question $F1738', 'chat-question-outline $F1739', 'message-question $F173A',
    'message-question-outline $F173B', 'kettle-pour-over $F173C', 'message-reply-outline $F173D',
    'message-reply-text-outline $F173E', 'koala $F173F', 'check-decagram-outline $F1740',
    'star-shooting $F1741', 'star-shooting-outline $F1742', 'table-picnic $F1743',
    'kitesurfing $F1744', 'paragliding $F1745', 'surfing $F1746',
    'floor-lamp-torchiere $F1747', 'mortar-pestle $F1748', 'cast-audio-variant $F1749',
    'gradient-horizontal $F174A', 'archive-cancel $F174B', 'archive-cancel-outline $F174C',
    'archive-check $F174D', 'archive-check-outline $F174E', 'archive-clock $F174F',
    'archive-clock-outline $F1750', 'archive-cog $F1751', 'archive-cog-outline $F1752',
    'archive-edit $F1753', 'archive-edit-outline $F1754', 'archive-eye $F1755',
    'archive-eye-outline $F1756', 'archive-lock $F1757', 'archive-lock-open $F1758',
    'archive-lock-open-outline $F1759', 'archive-lock-outline $F175A', 'archive-marker $F175B',
    'archive-marker-outline $F175C', 'archive-minus $F175D', 'archive-minus-outline $F175E',
    'archive-music $F175F', 'archive-music-outline $F1760', 'archive-off $F1761',
    'archive-off-outline $F1762', 'archive-plus $F1763', 'archive-plus-outline $F1764',
    'archive-refresh $F1765', 'archive-refresh-outline $F1766', 'archive-remove $F1767',
    'archive-remove-outline $F1768', 'archive-search $F1769', 'archive-search-outline $F176A',
    'archive-settings $F176B', 'archive-settings-outline $F176C', 'archive-star $F176D',
    'archive-star-outline $F176E', 'archive-sync $F176F', 'archive-sync-outline $F1770',
    'brush-off $F1771', 'file-image-marker $F1772', 'file-image-marker-outline $F1773',
    'file-marker $F1774', 'file-marker-outline $F1775', 'hamburger-check $F1776',
    'hamburger-minus $F1777', 'hamburger-off $F1778', 'hamburger-plus $F1779',
    'hamburger-remove $F177A', 'image-marker $F177B', 'image-marker-outline $F177C',
    'note-alert $F177D', 'note-alert-outline $F177E', 'note-check $F177F',
    'note-check-outline $F1780', 'note-edit $F1781', 'note-edit-outline $F1782',
    'note-off $F1783', 'note-off-outline $F1784', 'printer-off-outline $F1785',
    'printer-outline $F1786', 'progress-pencil $F1787', 'progress-star $F1788',
    'sausage-off $F1789', 'folder-eye $F178A', 'folder-eye-outline $F178B',
    'information-off $F178C', 'information-off-outline $F178D', 'sticker-text $F178E',
    'sticker-text-outline $F178F', 'web-cancel $F1790', 'web-refresh $F1791',
    'web-sync $F1792', 'chandelier $F1793', 'home-switch $F1794',
    'home-switch-outline $F1795', 'sun-snowflake $F1796', 'ceiling-fan $F1797',
    'ceiling-fan-light $F1798', 'smoke $F1799', 'fence $F179A',
    'light-recessed $F179B', 'battery-lock $F179C', 'battery-lock-open $F179D',
    'folder-hidden $F179E', 'mirror-rectangle $F179F', 'mirror-variant $F17A0',
    'arrow-down-left $F17A1', 'arrow-down-left-bold $F17A2', 'arrow-down-right $F17A3',
    'arrow-down-right-bold $F17A4', 'arrow-left-bottom $F17A5', 'arrow-left-bottom-bold $F17A6',
    'arrow-left-top $F17A7', 'arrow-left-top-bold $F17A8', 'arrow-right-bottom $F17A9',
    'arrow-right-bottom-bold $F17AA', 'arrow-right-top $F17AB', 'arrow-right-top-bold $F17AC',
    'arrow-u-down-left $F17AD', 'arrow-u-down-left-bold $F17AE', 'arrow-u-down-right $F17AF',
    'arrow-u-down-right-bold $F17B0', 'arrow-u-left-bottom $F17B1', 'arrow-u-left-bottom-bold $F17B2',
    'arrow-u-left-top $F17B3', 'arrow-u-left-top-bold $F17B4', 'arrow-u-right-bottom $F17B5',
    'arrow-u-right-bottom-bold $F17B6', 'arrow-u-right-top $F17B7', 'arrow-u-right-top-bold $F17B8',
    'arrow-u-up-left $F17B9', 'arrow-u-up-left-bold $F17BA', 'arrow-u-up-right $F17BB',
    'arrow-u-up-right-bold $F17BC', 'arrow-up-left $F17BD', 'arrow-up-left-bold $F17BE',
    'arrow-up-right $F17BF', 'arrow-up-right-bold $F17C0', 'select-remove $F17C1',
    'selection-ellipse-remove $F17C2', 'selection-remove $F17C3', 'human-greeting $F17C4',
    'ph $F17C5', 'water-sync $F17C6', 'ceiling-light-outline $F17C7',
    'floor-lamp-outline $F17C8', 'wall-sconce-flat-outline $F17C9', 'wall-sconce-flat-variant-outline $F17CA',
    'wall-sconce-outline $F17CB', 'wall-sconce-round-outline $F17CC', 'wall-sconce-round-variant-outline $F17CD',
    'floor-lamp-dual-outline $F17CE', 'floor-lamp-torchiere-variant-outline $F17CF', 'lamp-outline $F17D0',
    'lamps-outline $F17D1', 'candelabra $F17D2', 'candelabra-fire $F17D3',
    'menorah $F17D4', 'menorah-fire $F17D5', 'floor-lamp-torchiere-outline $F17D6',
    'credit-card-edit $F17D7', 'credit-card-edit-outline $F17D8', 'briefcase-eye $F17D9',
    'briefcase-eye-outline $F17DA', 'soundbar $F17DB', 'crown-circle $F17DC',
    'crown-circle-outline $F17DD', 'battery-arrow-down $F17DE', 'battery-arrow-down-outline $F17DF',
    'battery-arrow-up $F17E0', 'battery-arrow-up-outline $F17E1', 'battery-check $F17E2',
    'battery-check-outline $F17E3', 'battery-minus $F17E4', 'battery-minus-outline $F17E5',
    'battery-plus $F17E6', 'battery-plus-outline $F17E7', 'battery-remove $F17E8',
    'battery-remove-outline $F17E9', 'chili-alert $F17EA', 'chili-alert-outline $F17EB',
    'chili-hot-outline $F17EC', 'chili-medium-outline $F17ED', 'chili-mild-outline $F17EE',
    'chili-off-outline $F17EF', 'cake-variant-outline $F17F0', 'card-multiple $F17F1',
    'card-multiple-outline $F17F2', 'account-cowboy-hat-outline $F17F3', 'lightbulb-spot $F17F4',
    'lightbulb-spot-off $F17F5', 'fence-electric $F17F6', 'gate-arrow-left $F17F7',
    'gate-alert $F17F8', 'boom-gate-up $F17F9', 'boom-gate-up-outline $F17FA',
    'garage-lock $F17FB', 'garage-variant-lock $F17FC', 'cellphone-check $F17FD',
    'sun-wireless $F17FE', 'sun-wireless-outline $F17FF', 'lightbulb-auto $F1800',
    'lightbulb-auto-outline $F1801', 'lightbulb-variant $F1802', 'lightbulb-variant-outline $F1803',
    'lightbulb-fluorescent-tube $F1804', 'lightbulb-fluorescent-tube-outline $F1805', 'water-circle $F1806',
    'fire-circle $F1807', 'smoke-detector-outline $F1808', 'smoke-detector-off $F1809',
    'smoke-detector-off-outline $F180A', 'smoke-detector-variant $F180B', 'smoke-detector-variant-off $F180C',
    'projector-screen-off $F180D', 'projector-screen-off-outline $F180E', 'projector-screen-variant $F180F',
    'projector-screen-variant-off $F1810', 'projector-screen-variant-off-outline $F1811', 'projector-screen-variant-outline $F1812',
    'brush-variant $F1813', 'car-wrench $F1814', 'account-injury $F1815',
    'account-injury-outline $F1816', 'balcony $F1817', 'bathtub $F1818',
    'bathtub-outline $F1819', 'blender-outline $F181A', 'coffee-maker-outline $F181B',
    'countertop $F181C', 'countertop-outline $F181D', 'door-sliding $F181E',
    'door-sliding-lock $F181F', 'door-sliding-open $F1820', 'hand-wave $F1821',
    'hand-wave-outline $F1822', 'human-male-female-child $F1823', 'iron $F1824',
    'iron-outline $F1825', 'liquid-spot $F1826', 'mosque $F1827',
    'shield-moon $F1828', 'shield-moon-outline $F1829', 'traffic-light-outline $F182A',
    'hand-front-left $F182B', 'hand-back-left-outline $F182C', 'hand-back-right-outline $F182D',
    'hand-front-left-outline $F182E', 'hand-front-right-outline $F182F', 'hand-back-left-off $F1830',
    'hand-back-right-off $F1831', 'hand-back-left-off-outline $F1832', 'hand-back-right-off-outline $F1833',
    'battery-sync $F1834', 'battery-sync-outline $F1835', 'food-takeout-box $F1836',
    'food-takeout-box-outline $F1837', 'iron-board $F1838', 'police-station $F1839',
    'cellphone-marker $F183A', 'tooltip-cellphone $F183B', 'table-pivot $F183C',
    'tunnel $F183D', 'tunnel-outline $F183E', 'arrow-projectile-multiple $F183F',
    'arrow-projectile $F1840', 'bow-arrow $F1841', 'axe-battle $F1842',
    'mace $F1843', 'magic-staff $F1844', 'spear $F1845',
    'curtains $F1846', 'curtains-closed $F1847', 'human-non-binary $F1848',
    'waterfall $F1849', 'egg-fried $F184A', 'food-hot-dog $F184B',
    'induction $F184C', 'pipe-valve $F184D', 'shipping-pallet $F184E',
    'earbuds $F184F', 'earbuds-off $F1850', 'earbuds-off-outline $F1851',
    'earbuds-outline $F1852', 'circle-opacity $F1853', 'square-opacity $F1854',
    'water-opacity $F1855', 'vector-polygon-variant $F1856', 'vector-square-close $F1857',
    'vector-square-open $F1858', 'waves-arrow-left $F1859', 'waves-arrow-right $F185A',
    'waves-arrow-up $F185B', 'cash-fast $F185C', 'radioactive-circle $F185D',
    'radioactive-circle-outline $F185E', 'cctv-off $F185F', 'format-list-group $F1860',
    'clock-plus $F1861', 'clock-plus-outline $F1862', 'clock-minus $F1863',
    'clock-minus-outline $F1864', 'clock-remove $F1865', 'clock-remove-outline $F1866',
    'account-arrow-up $F1867', 'account-arrow-down $F1868', 'account-arrow-down-outline $F1869',
    'account-arrow-up-outline $F186A', 'audio-input-rca $F186B', 'audio-input-stereo-minijack $F186C',
    'audio-input-xlr $F186D', 'horse-variant-fast $F186E', 'email-fast $F186F',
    'email-fast-outline $F1870', 'camera-document $F1871', 'camera-document-off $F1872',
    'glass-fragile $F1873', 'magnify-expand $F1874', 'town-hall $F1875',
    'monitor-small $F1876', 'diversify $F1877', 'car-wireless $F1878',
    'car-select $F1879', 'airplane-alert $F187A', 'airplane-check $F187B',
    'airplane-clock $F187C', 'airplane-cog $F187D', 'airplane-edit $F187E',
    'airplane-marker $F187F', 'airplane-minus $F1880', 'airplane-plus $F1881',
    'airplane-remove $F1882', 'airplane-search $F1883', 'airplane-settings $F1884',
    'flower-pollen $F1885', 'flower-pollen-outline $F1886', 'hammer-sickle $F1887',
    'view-gallery $F1888', 'view-gallery-outline $F1889', 'umbrella-beach $F188A',
    'umbrella-beach-outline $F188B', 'cabin-a-frame $F188C', 'all-inclusive-box $F188D',
    'all-inclusive-box-outline $F188E', 'hand-coin $F188F', 'hand-coin-outline $F1890',
    'truck-flatbed $F1891', 'layers-edit $F1892', 'multicast $F1893',
    'hydrogen-station $F1894', 'thermometer-bluetooth $F1895', 'tire $F1896',
    'forest $F1897', 'account-tie-hat $F1898', 'account-tie-hat-outline $F1899',
    'account-wrench $F189A', 'account-wrench-outline $F189B', 'bicycle-cargo $F189C',
    'calendar-collapse-horizontal $F189D', 'calendar-expand-horizontal $F189E', 'cards-club-outline $F189F',
    'cards-playing $F18A1', 'cards-playing-club $F18A2', 'cards-playing-club-multiple $F18A3',
    'cards-playing-club-multiple-outline $F18A4', 'cards-playing-club-outline $F18A5', 'cards-playing-diamond $F18A6',
    'cards-playing-diamond-multiple $F18A7', 'cards-playing-diamond-multiple-outline $F18A8', 'cards-playing-diamond-outline $F18A9',
    'cards-playing-heart $F18AA', 'cards-playing-heart-multiple $F18AB', 'cards-playing-heart-multiple-outline $F18AC',
    'cards-playing-heart-outline $F18AD', 'cards-playing-spade $F18AE', 'cards-playing-spade-multiple $F18AF',
    'cards-playing-spade-multiple-outline $F18B0', 'cards-playing-spade-outline $F18B1', 'cards-spade-outline $F18B2',
    'compare-remove $F18B3', 'dolphin $F18B4', 'fuel-cell $F18B5',
    'hand-extended $F18B6', 'hand-extended-outline $F18B7', 'printer-3d-nozzle-heat $F18B8',
    'printer-3d-nozzle-heat-outline $F18B9', 'shark $F18BA', 'shark-off $F18BB',
    'shield-crown $F18BC', 'shield-crown-outline $F18BD', 'shield-sword $F18BE',
    'shield-sword-outline $F18BF', 'sickle $F18C0', 'store-alert $F18C1',
    'store-alert-outline $F18C2', 'store-check $F18C3', 'store-check-outline $F18C4',
    'store-clock $F18C5', 'store-clock-outline $F18C6', 'store-cog $F18C7',
    'store-cog-outline $F18C8', 'store-edit $F18C9', 'store-edit-outline $F18CA',
    'store-marker $F18CB', 'store-marker-outline $F18CC', 'store-minus-outline $F18CD',
    'store-off $F18CE', 'store-off-outline $F18CF', 'store-plus-outline $F18D0',
    'store-remove-outline $F18D1', 'store-search $F18D2', 'store-search-outline $F18D3',
    'store-settings $F18D4', 'store-settings-outline $F18D5', 'sun-thermometer $F18D6',
    'sun-thermometer-outline $F18D7', 'truck-cargo-container $F18D8', 'vector-square-edit $F18D9',
    'vector-square-minus $F18DA', 'vector-square-plus $F18DB', 'vector-square-remove $F18DC',
    'ceiling-light-multiple $F18DD', 'ceiling-light-multiple-outline $F18DE', 'wiper-wash-alert $F18DF',
    'cart-heart $F18E0', 'virus-off $F18E1', 'virus-off-outline $F18E2',
    'map-marker-account $F18E3', 'map-marker-account-outline $F18E4', 'basket-check $F18E5',
    'basket-check-outline $F18E6', 'credit-card-lock $F18E7', 'credit-card-lock-outline $F18E8',
    'format-underline-wavy $F18E9', 'content-save-check $F18EA', 'content-save-check-outline $F18EB',
    'filter-check $F18EC', 'filter-check-outline $F18ED', 'flag-off $F18EE',
    'flag-off-outline $F18EF', 'navigation-variant-outline $F18F1', 'refresh-auto $F18F2',
    'tilde-off $F18F3', 'fit-to-screen $F18F4', 'fit-to-screen-outline $F18F5',
    'weather-cloudy-clock $F18F6', 'smart-card-off $F18F7', 'smart-card-off-outline $F18F8',
    'clipboard-text-clock $F18F9', 'clipboard-text-clock-outline $F18FA', 'teddy-bear $F18FB',
    'cow-off $F18FC', 'eye-arrow-left $F18FD', 'eye-arrow-left-outline $F18FE',
    'eye-arrow-right $F18FF', 'eye-arrow-right-outline $F1900', 'home-battery $F1901',
    'home-battery-outline $F1902', 'home-lightning-bolt $F1903', 'home-lightning-bolt-outline $F1904',
    'leaf-circle $F1905', 'leaf-circle-outline $F1906', 'tag-search $F1907',
    'tag-search-outline $F1908', 'car-brake-fluid-level $F1909', 'car-brake-low-pressure $F190A',
    'car-brake-temperature $F190B', 'car-brake-worn-linings $F190C', 'car-light-alert $F190D',
    'car-speed-limiter $F190E', 'credit-card-chip $F190F', 'credit-card-chip-outline $F1910',
    'credit-card-fast $F1911', 'credit-card-fast-outline $F1912', 'integrated-circuit-chip $F1913',
    'thumbs-up-down-outline $F1914', 'food-off-outline $F1915', 'food-outline $F1916',
    'format-page-split $F1917', 'chart-waterfall $F1918', 'gamepad-outline $F1919',
    'network-strength-4-cog $F191A', 'account-sync $F191B', 'account-sync-outline $F191C',
    'bus-electric $F191D', 'liquor $F191E', 'database-eye $F191F',
    'database-eye-off $F1920', 'database-eye-off-outline $F1921', 'database-eye-outline $F1922',
    'timer-settings $F1923', 'timer-settings-outline $F1924', 'timer-cog $F1925',
    'timer-cog-outline $F1926', 'checkbox-marked-circle-plus-outline $F1927', 'panorama-horizontal $F1928',
    'panorama-vertical $F1929', 'advertisements $F192A', 'advertisements-off $F192B',
    'transmission-tower-export $F192C', 'transmission-tower-import $F192D', 'smoke-detector-alert $F192E',
    'smoke-detector-alert-outline $F192F', 'smoke-detector-variant-alert $F1930', 'coffee-maker-check $F1931',
    'coffee-maker-check-outline $F1932', 'cog-pause $F1933', 'cog-pause-outline $F1934',
    'cog-play $F1935', 'cog-play-outline $F1936', 'cog-stop $F1937',
    'cog-stop-outline $F1938', 'copyleft $F1939', 'fast-forward-15 $F193A',
    'file-image-minus $F193B', 'file-image-minus-outline $F193C', 'file-image-plus $F193D',
    'file-image-plus-outline $F193E', 'file-image-remove $F193F', 'file-image-remove-outline $F1940',
    'message-badge $F1941', 'message-badge-outline $F1942', 'newspaper-check $F1943',
    'newspaper-remove $F1944', 'publish-off $F1945', 'rewind-15 $F1946',
    'view-dashboard-edit $F1947', 'view-dashboard-edit-outline $F1948', 'office-building-cog $F1949',
    'office-building-cog-outline $F194A', 'hand-clap $F194B', 'cone $F194C',
    'cone-off $F194D', 'cylinder $F194E', 'cylinder-off $F194F',
    'octahedron $F1950', 'octahedron-off $F1951', 'pyramid $F1952',
    'pyramid-off $F1953', 'sphere $F1954', 'sphere-off $F1955',
    'format-letter-spacing $F1956', 'french-fries $F1957', 'scent $F1958',
    'scent-off $F1959', 'palette-swatch-variant $F195A', 'email-seal $F195B',
    'email-seal-outline $F195C', 'stool $F195D', 'stool-outline $F195E',
    'panorama-wide-angle $F195F'];

const
  glyphVectorSquare = $F0001; glyphAccessPointNetwork = $F0002; glyphAccessPoint = $F0003;
  glyphAccount = $F0004; glyphAccountAlert = $F0005; glyphAccountBox = $F0006;
  glyphAccountBoxOutline = $F0007; glyphAccountCheck = $F0008; glyphAccountCircle = $F0009;
  glyphAccountConvert = $F000A; glyphAccountKey = $F000B; glyphTooltipAccount = $F000C;
  glyphAccountMinus = $F000D; glyphAccountMultiple = $F000E; glyphAccountMultipleOutline = $F000F;
  glyphAccountMultiplePlus = $F0010; glyphAccountNetwork = $F0011; glyphAccountOff = $F0012;
  glyphAccountOutline = $F0013; glyphAccountPlus = $F0014; glyphAccountRemove = $F0015;
  glyphAccountSearch = $F0016; glyphAccountStar = $F0017; glyphOrbit = $F0018;
  glyphAccountSwitch = $F0019; glyphAdjust = $F001A; glyphAirConditioner = $F001B;
  glyphAirballoon = $F001C; glyphAirplane = $F001D; glyphAirplaneOff = $F001E;
  glyphCastVariant = $F001F; glyphAlarm = $F0020; glyphAlarmCheck = $F0021;
  glyphAlarmMultiple = $F0022; glyphAlarmOff = $F0023; glyphAlarmPlus = $F0024;
  glyphAlbum = $F0025; glyphAlert = $F0026; glyphAlertBox = $F0027;
  glyphAlertCircle = $F0028; glyphAlertOctagon = $F0029; glyphAlertOutline = $F002A;
  glyphAlpha = $F002B; glyphAlphabetical = $F002C; glyphGreenhouse = $F002D;
  glyphRollerbladeOff = $F002E; glyphAmbulance = $F002F; glyphAmplifier = $F0030;
  glyphAnchor = $F0031; glyphAndroid = $F0032; glyphWebPlus = $F0033;
  glyphAndroidStudio = $F0034; glyphApple = $F0035; glyphAppleFinder = $F0036;
  glyphAppleIos = $F0037; glyphAppleIcloud = $F0038; glyphAppleSafari = $F0039;
  glyphFontAwesome = $F003A; glyphApps = $F003B; glyphArchive = $F003C;
  glyphArrangeBringForward = $F003D; glyphArrangeBringToFront = $F003E; glyphArrangeSendBackward = $F003F;
  glyphArrangeSendToBack = $F0040; glyphArrowAll = $F0041; glyphArrowBottomLeft = $F0042;
  glyphArrowBottomRight = $F0043; glyphArrowCollapseAll = $F0044; glyphArrowDown = $F0045;
  glyphArrowDownThick = $F0046; glyphArrowDownBoldCircle = $F0047; glyphArrowDownBoldCircleOutline = $F0048;
  glyphArrowDownBoldHexagonOutline = $F0049; glyphArrowDownDropCircle = $F004A; glyphArrowDownDropCircleOutline = $F004B;
  glyphArrowExpandAll = $F004C; glyphArrowLeft = $F004D; glyphArrowLeftThick = $F004E;
  glyphArrowLeftBoldCircle = $F004F; glyphArrowLeftBoldCircleOutline = $F0050; glyphArrowLeftBoldHexagonOutline = $F0051;
  glyphArrowLeftDropCircle = $F0052; glyphArrowLeftDropCircleOutline = $F0053; glyphArrowRight = $F0054;
  glyphArrowRightThick = $F0055; glyphArrowRightBoldCircle = $F0056; glyphArrowRightBoldCircleOutline = $F0057;
  glyphArrowRightBoldHexagonOutline = $F0058; glyphArrowRightDropCircle = $F0059; glyphArrowRightDropCircleOutline = $F005A;
  glyphArrowTopLeft = $F005B; glyphArrowTopRight = $F005C; glyphArrowUp = $F005D;
  glyphArrowUpThick = $F005E; glyphArrowUpBoldCircle = $F005F; glyphArrowUpBoldCircleOutline = $F0060;
  glyphArrowUpBoldHexagonOutline = $F0061; glyphArrowUpDropCircle = $F0062; glyphArrowUpDropCircleOutline = $F0063;
  glyphAssistant = $F0064; glyphAt = $F0065; glyphAttachment = $F0066;
  glyphBookMusic = $F0067; glyphAutoFix = $F0068; glyphAutoUpload = $F0069;
  glyphAutorenew = $F006A; glyphAvTimer = $F006B; glyphBaby = $F006C;
  glyphBackburger = $F006D; glyphBackspace = $F006E; glyphBackupRestore = $F006F;
  glyphBank = $F0070; glyphBarcode = $F0071; glyphBarcodeScan = $F0072;
  glyphBarley = $F0073; glyphBarrel = $F0074; glyphIncognitoOff = $F0075;
  glyphBasket = $F0076; glyphBasketFill = $F0077; glyphBasketUnfill = $F0078;
  glyphBattery = $F0079; glyphBattery10 = $F007A; glyphBattery20 = $F007B;
  glyphBattery30 = $F007C; glyphBattery40 = $F007D; glyphBattery50 = $F007E;
  glyphBattery60 = $F007F; glyphBattery70 = $F0080; glyphBattery80 = $F0081;
  glyphBattery90 = $F0082; glyphBatteryAlert = $F0083; glyphBatteryCharging = $F0084;
  glyphBatteryCharging100 = $F0085; glyphBatteryCharging20 = $F0086; glyphBatteryCharging30 = $F0087;
  glyphBatteryCharging40 = $F0088; glyphBatteryCharging60 = $F0089; glyphBatteryCharging80 = $F008A;
  glyphBatteryCharging90 = $F008B; glyphBatteryMinusVariant = $F008C; glyphBatteryNegative = $F008D;
  glyphBatteryOutline = $F008E; glyphBatteryPlusVariant = $F008F; glyphBatteryPositive = $F0090;
  glyphBatteryUnknown = $F0091; glyphBeach = $F0092; glyphFlask = $F0093;
  glyphFlaskEmpty = $F0094; glyphFlaskEmptyOutline = $F0095; glyphFlaskOutline = $F0096;
  glyphBunkBedOutline = $F0097; glyphBeer = $F0098; glyphBedOutline = $F0099;
  glyphBell = $F009A; glyphBellOff = $F009B; glyphBellOutline = $F009C;
  glyphBellPlus = $F009D; glyphBellRing = $F009E; glyphBellRingOutline = $F009F;
  glyphBellSleep = $F00A0; glyphBeta = $F00A1; glyphBookCross = $F00A2;
  glyphBike = $F00A3; glyphMicrosoftBing = $F00A4; glyphBinoculars = $F00A5;
  glyphBio = $F00A6; glyphBiohazard = $F00A7; glyphBitbucket = $F00A8;
  glyphBlackMesa = $F00A9; glyphShieldRefresh = $F00AA; glyphBlenderSoftware = $F00AB;
  glyphBlinds = $F00AC; glyphBlockHelper = $F00AD; glyphApplicationEdit = $F00AE;
  glyphBluetooth = $F00AF; glyphBluetoothAudio = $F00B0; glyphBluetoothConnect = $F00B1;
  glyphBluetoothOff = $F00B2; glyphBluetoothSettings = $F00B3; glyphBluetoothTransfer = $F00B4;
  glyphBlur = $F00B5; glyphBlurLinear = $F00B6; glyphBlurOff = $F00B7;
  glyphBlurRadial = $F00B8; glyphBone = $F00B9; glyphBook = $F00BA;
  glyphBookMultiple = $F00BB; glyphBookVariantMultiple = $F00BC; glyphBookOpen = $F00BD;
  glyphBookOpenBlankVariant = $F00BE; glyphBookVariant = $F00BF; glyphBookmark = $F00C0;
  glyphBookmarkCheck = $F00C1; glyphBookmarkMusic = $F00C2; glyphBookmarkOutline = $F00C3;
  glyphBookmarkPlusOutline = $F00C4; glyphBookmarkPlus = $F00C5; glyphBookmarkRemove = $F00C6;
  glyphBorderAll = $F00C7; glyphBorderBottom = $F00C8; glyphBorderColor = $F00C9;
  glyphBorderHorizontal = $F00CA; glyphBorderInside = $F00CB; glyphBorderLeft = $F00CC;
  glyphBorderNone = $F00CD; glyphBorderOutside = $F00CE; glyphBorderRight = $F00CF;
  glyphBorderStyle = $F00D0; glyphBorderTop = $F00D1; glyphBorderVertical = $F00D2;
  glyphBowling = $F00D3; glyphBox = $F00D4; glyphBoxCutter = $F00D5;
  glyphBriefcase = $F00D6; glyphBriefcaseCheck = $F00D7; glyphBriefcaseDownload = $F00D8;
  glyphBriefcaseUpload = $F00D9; glyphBrightness1 = $F00DA; glyphBrightness2 = $F00DB;
  glyphBrightness3 = $F00DC; glyphBrightness4 = $F00DD; glyphBrightness5 = $F00DE;
  glyphBrightness6 = $F00DF; glyphBrightness7 = $F00E0; glyphBrightnessAuto = $F00E1;
  glyphBroom = $F00E2; glyphBrush = $F00E3; glyphBug = $F00E4;
  glyphBulletinBoard = $F00E5; glyphBullhorn = $F00E6; glyphBus = $F00E7;
  glyphCached = $F00E8; glyphCake = $F00E9; glyphCakeLayered = $F00EA;
  glyphCakeVariant = $F00EB; glyphCalculator = $F00EC; glyphCalendar = $F00ED;
  glyphCalendarBlank = $F00EE; glyphCalendarCheck = $F00EF; glyphCalendarClock = $F00F0;
  glyphCalendarMultiple = $F00F1; glyphCalendarMultipleCheck = $F00F2; glyphCalendarPlus = $F00F3;
  glyphCalendarRemove = $F00F4; glyphCalendarText = $F00F5; glyphCalendarToday = $F00F6;
  glyphCallMade = $F00F7; glyphCallMerge = $F00F8; glyphCallMissed = $F00F9;
  glyphCallReceived = $F00FA; glyphCallSplit = $F00FB; glyphCamcorder = $F00FC;
  glyphVideoBox = $F00FD; glyphVideoBoxOff = $F00FE; glyphCamcorderOff = $F00FF;
  glyphCamera = $F0100; glyphCameraEnhance = $F0101; glyphCameraFront = $F0102;
  glyphCameraFrontVariant = $F0103; glyphCameraIris = $F0104; glyphCameraPartyMode = $F0105;
  glyphCameraRear = $F0106; glyphCameraRearVariant = $F0107; glyphCameraSwitch = $F0108;
  glyphCameraTimer = $F0109; glyphCandycane = $F010A; glyphCar = $F010B;
  glyphCarBattery = $F010C; glyphCarConnected = $F010D; glyphCarWash = $F010E;
  glyphCarrot = $F010F; glyphCart = $F0110; glyphCartOutline = $F0111;
  glyphCartPlus = $F0112; glyphCaseSensitiveAlt = $F0113; glyphCash = $F0114;
  glyphCash100 = $F0115; glyphCashMultiple = $F0116; glyphCheckboxBlankBadgeOutline = $F0117;
  glyphCast = $F0118; glyphCastConnected = $F0119; glyphCastle = $F011A;
  glyphCat = $F011B; glyphCellphone = $F011C; glyphTrayArrowUp = $F011D;
  glyphCellphoneBasic = $F011E; glyphCellphoneDock = $F011F; glyphTrayArrowDown = $F0120;
  glyphCellphoneLink = $F0121; glyphCellphoneLinkOff = $F0122; glyphCellphoneSettings = $F0123;
  glyphCertificate = $F0124; glyphChairSchool = $F0125; glyphChartArc = $F0126;
  glyphChartAreaspline = $F0127; glyphChartBar = $F0128; glyphChartHistogram = $F0129;
  glyphChartLine = $F012A; glyphChartPie = $F012B; glyphCheck = $F012C;
  glyphCheckAll = $F012D; glyphCheckboxBlank = $F012E; glyphCheckboxBlankCircle = $F012F;
  glyphCheckboxBlankCircleOutline = $F0130; glyphCheckboxBlankOutline = $F0131; glyphCheckboxMarked = $F0132;
  glyphCheckboxMarkedCircle = $F0133; glyphCheckboxMarkedCircleOutline = $F0134; glyphCheckboxMarkedOutline = $F0135;
  glyphCheckboxMultipleBlank = $F0136; glyphCheckboxMultipleBlankOutline = $F0137; glyphCheckboxMultipleMarked = $F0138;
  glyphCheckboxMultipleMarkedOutline = $F0139; glyphCheckerboard = $F013A; glyphChemicalWeapon = $F013B;
  glyphChevronDoubleDown = $F013C; glyphChevronDoubleLeft = $F013D; glyphChevronDoubleRight = $F013E;
  glyphChevronDoubleUp = $F013F; glyphChevronDown = $F0140; glyphChevronLeft = $F0141;
  glyphChevronRight = $F0142; glyphChevronUp = $F0143; glyphChurch = $F0144;
  glyphRollerSkateOff = $F0145; glyphCity = $F0146; glyphClipboard = $F0147;
  glyphClipboardAccount = $F0148; glyphClipboardAlert = $F0149; glyphClipboardArrowDown = $F014A;
  glyphClipboardArrowLeft = $F014B; glyphClipboardOutline = $F014C; glyphClipboardText = $F014D;
  glyphClipboardCheck = $F014E; glyphClippy = $F014F; glyphClockOutline = $F0150;
  glyphClockEnd = $F0151; glyphClockFast = $F0152; glyphClockIn = $F0153;
  glyphClockOut = $F0154; glyphClockStart = $F0155; glyphClose = $F0156;
  glyphCloseBox = $F0157; glyphCloseBoxOutline = $F0158; glyphCloseCircle = $F0159;
  glyphCloseCircleOutline = $F015A; glyphCloseNetwork = $F015B; glyphCloseOctagon = $F015C;
  glyphCloseOctagonOutline = $F015D; glyphClosedCaption = $F015E; glyphCloud = $F015F;
  glyphCloudCheck = $F0160; glyphCloudCircle = $F0161; glyphCloudDownload = $F0162;
  glyphCloudOutline = $F0163; glyphCloudOffOutline = $F0164; glyphCloudPrint = $F0165;
  glyphCloudPrintOutline = $F0166; glyphCloudUpload = $F0167; glyphCodeArray = $F0168;
  glyphCodeBraces = $F0169; glyphCodeBrackets = $F016A; glyphCodeEqual = $F016B;
  glyphCodeGreaterThan = $F016C; glyphCodeGreaterThanOrEqual = $F016D; glyphCodeLessThan = $F016E;
  glyphCodeLessThanOrEqual = $F016F; glyphCodeNotEqual = $F0170; glyphCodeNotEqualVariant = $F0171;
  glyphCodeParentheses = $F0172; glyphCodeString = $F0173; glyphCodeTags = $F0174;
  glyphCodepen = $F0175; glyphCoffee = $F0176; glyphCoffeeToGo = $F0177;
  glyphBellBadgeOutline = $F0178; glyphColorHelper = $F0179; glyphComment = $F017A;
  glyphCommentAccount = $F017B; glyphCommentAccountOutline = $F017C; glyphCommentAlert = $F017D;
  glyphCommentAlertOutline = $F017E; glyphCommentCheck = $F017F; glyphCommentCheckOutline = $F0180;
  glyphCommentMultipleOutline = $F0181; glyphCommentOutline = $F0182; glyphCommentPlusOutline = $F0183;
  glyphCommentProcessing = $F0184; glyphCommentProcessingOutline = $F0185; glyphCommentQuestionOutline = $F0186;
  glyphCommentRemoveOutline = $F0187; glyphCommentText = $F0188; glyphCommentTextOutline = $F0189;
  glyphCompare = $F018A; glyphCompass = $F018B; glyphCompassOutline = $F018C;
  glyphConsole = $F018D; glyphCardAccountMail = $F018E; glyphContentCopy = $F018F;
  glyphContentCut = $F0190; glyphContentDuplicate = $F0191; glyphContentPaste = $F0192;
  glyphContentSave = $F0193; glyphContentSaveAll = $F0194; glyphContrast = $F0195;
  glyphContrastBox = $F0196; glyphContrastCircle = $F0197; glyphCookie = $F0198;
  glyphCounter = $F0199; glyphCow = $F019A; glyphCreditCardOutline = $F019B;
  glyphCreditCardMultipleOutline = $F019C; glyphCreditCardScanOutline = $F019D; glyphCrop = $F019E;
  glyphCropFree = $F019F; glyphCropLandscape = $F01A0; glyphCropPortrait = $F01A1;
  glyphCropSquare = $F01A2; glyphCrosshairs = $F01A3; glyphCrosshairsGps = $F01A4;
  glyphCrown = $F01A5; glyphCube = $F01A6; glyphCubeOutline = $F01A7;
  glyphCubeSend = $F01A8; glyphCubeUnfolded = $F01A9; glyphCup = $F01AA;
  glyphCupWater = $F01AB; glyphCurrencyBtc = $F01AC; glyphCurrencyEur = $F01AD;
  glyphCurrencyGbp = $F01AE; glyphCurrencyInr = $F01AF; glyphCurrencyNgn = $F01B0;
  glyphCurrencyRub = $F01B1; glyphCurrencyTry = $F01B2; glyphDeleteVariant = $F01B3;
  glyphDelete = $F01B4; glyphDecimalIncrease = $F01B5; glyphDecimalDecrease = $F01B6;
  glyphDebugStepOver = $F01B7; glyphDebugStepOut = $F01B8; glyphDebugStepInto = $F01B9;
  glyphDatabasePlus = $F01BA; glyphDatabaseMinus = $F01BB; glyphDatabase = $F01BC;
  glyphCursorPointer = $F01BD; glyphCursorMove = $F01BE; glyphCursorDefaultOutline = $F01BF;
  glyphCursorDefault = $F01C0; glyphCurrencyUsd = $F01C1; glyphDelta = $F01C2;
  glyphDeskphone = $F01C3; glyphDesktopMac = $F01C4; glyphDesktopTower = $F01C5;
  glyphDetails = $F01C6; glyphDeviantart = $F01C7; glyphDiamondStone = $F01C8;
  glyphAbTesting = $F01C9; glyphDice1 = $F01CA; glyphDice2 = $F01CB;
  glyphDice3 = $F01CC; glyphDice4 = $F01CD; glyphDice5 = $F01CE;
  glyphDice6 = $F01CF; glyphDirections = $F01D0; glyphDiscAlert = $F01D1;
  glyphDisqus = $F01D2; glyphVideoPlusOutline = $F01D3; glyphDivision = $F01D4;
  glyphDivisionBox = $F01D5; glyphDns = $F01D6; glyphDomain = $F01D7;
  glyphDotsHorizontal = $F01D8; glyphDotsVertical = $F01D9; glyphDownload = $F01DA;
  glyphDrag = $F01DB; glyphDragHorizontal = $F01DC; glyphDragVertical = $F01DD;
  glyphDrawing = $F01DE; glyphDrawingBox = $F01DF; glyphShieldRefreshOutline = $F01E0;
  glyphCalendarRefresh = $F01E1; glyphDrone = $F01E2; glyphDropbox = $F01E3;
  glyphDrupal = $F01E4; glyphDuck = $F01E5; glyphDumbbell = $F01E6;
  glyphEarth = $F01E7; glyphEarthOff = $F01E8; glyphMicrosoftEdge = $F01E9;
  glyphEject = $F01EA; glyphElevationDecline = $F01EB; glyphElevationRise = $F01EC;
  glyphElevator = $F01ED; glyphEmail = $F01EE; glyphEmailOpen = $F01EF;
  glyphEmailOutline = $F01F0; glyphEmailLock = $F01F1; glyphEmoticonOutline = $F01F2;
  glyphEmoticonCoolOutline = $F01F3; glyphEmoticonDevilOutline = $F01F4; glyphEmoticonHappyOutline = $F01F5;
  glyphEmoticonNeutralOutline = $F01F6; glyphEmoticonPoop = $F01F7; glyphEmoticonSadOutline = $F01F8;
  glyphEmoticonTongue = $F01F9; glyphEngine = $F01FA; glyphEngineOutline = $F01FB;
  glyphEqual = $F01FC; glyphEqualBox = $F01FD; glyphEraser = $F01FE;
  glyphEscalator = $F01FF; glyphEthernet = $F0200; glyphEthernetCable = $F0201;
  glyphEthernetCableOff = $F0202; glyphCalendarRefreshOutline = $F0203; glyphEvernote = $F0204;
  glyphExclamation = $F0205; glyphExitToApp = $F0206; glyphExport = $F0207;
  glyphEye = $F0208; glyphEyeOff = $F0209; glyphEyedropper = $F020A;
  glyphEyedropperVariant = $F020B; glyphFacebook = $F020C; glyphOrderAlphabeticalAscending = $F020D;
  glyphFacebookMessenger = $F020E; glyphFactory = $F020F; glyphFan = $F0210;
  glyphFastForward = $F0211; glyphFax = $F0212; glyphFerry = $F0213;
  glyphFile = $F0214; glyphFileChart = $F0215; glyphFileCheck = $F0216;
  glyphFileCloud = $F0217; glyphFileDelimited = $F0218; glyphFileDocument = $F0219;
  glyphTextBox = $F021A; glyphFileExcel = $F021B; glyphFileExcelBox = $F021C;
  glyphFileExport = $F021D; glyphFileFind = $F021E; glyphFileImage = $F021F;
  glyphFileImport = $F0220; glyphFileLock = $F0221; glyphFileMultiple = $F0222;
  glyphFileMusic = $F0223; glyphFileOutline = $F0224; glyphFileJpgBox = $F0225;
  glyphFilePdfBox = $F0226; glyphFilePowerpoint = $F0227; glyphFilePowerpointBox = $F0228;
  glyphFilePresentationBox = $F0229; glyphFileSend = $F022A; glyphFileVideo = $F022B;
  glyphFileWord = $F022C; glyphFileWordBox = $F022D; glyphFileCode = $F022E;
  glyphFilm = $F022F; glyphFilmstrip = $F0230; glyphFilmstripOff = $F0231;
  glyphFilter = $F0232; glyphFilterOutline = $F0233; glyphFilterRemove = $F0234;
  glyphFilterRemoveOutline = $F0235; glyphFilterVariant = $F0236; glyphFingerprint = $F0237;
  glyphFire = $F0238; glyphFirefox = $F0239; glyphFish = $F023A;
  glyphFlag = $F023B; glyphFlagCheckered = $F023C; glyphFlagOutline = $F023D;
  glyphFlagVariantOutline = $F023E; glyphFlagTriangle = $F023F; glyphFlagVariant = $F0240;
  glyphFlash = $F0241; glyphFlashAuto = $F0242; glyphFlashOff = $F0243;
  glyphFlashlight = $F0244; glyphFlashlightOff = $F0245; glyphStarHalf = $F0246;
  glyphFlipToBack = $F0247; glyphFlipToFront = $F0248; glyphFloppy = $F0249;
  glyphFlower = $F024A; glyphFolder = $F024B; glyphFolderAccount = $F024C;
  glyphFolderDownload = $F024D; glyphFolderGoogleDrive = $F024E; glyphFolderImage = $F024F;
  glyphFolderLock = $F0250; glyphFolderLockOpen = $F0251; glyphFolderMove = $F0252;
  glyphFolderMultiple = $F0253; glyphFolderMultipleImage = $F0254; glyphFolderMultipleOutline = $F0255;
  glyphFolderOutline = $F0256; glyphFolderPlus = $F0257; glyphFolderRemove = $F0258;
  glyphFolderUpload = $F0259; glyphFood = $F025A; glyphFoodApple = $F025B;
  glyphFoodVariant = $F025C; glyphFootball = $F025D; glyphFootballAustralian = $F025E;
  glyphFootballHelmet = $F025F; glyphFormatAlignCenter = $F0260; glyphFormatAlignJustify = $F0261;
  glyphFormatAlignLeft = $F0262; glyphFormatAlignRight = $F0263; glyphFormatBold = $F0264;
  glyphFormatClear = $F0265; glyphFormatColorFill = $F0266; glyphFormatFloatCenter = $F0267;
  glyphFormatFloatLeft = $F0268; glyphFormatFloatNone = $F0269; glyphFormatFloatRight = $F026A;
  glyphFormatHeader1 = $F026B; glyphFormatHeader2 = $F026C; glyphFormatHeader3 = $F026D;
  glyphFormatHeader4 = $F026E; glyphFormatHeader5 = $F026F; glyphFormatHeader6 = $F0270;
  glyphFormatHeaderDecrease = $F0271; glyphFormatHeaderEqual = $F0272; glyphFormatHeaderIncrease = $F0273;
  glyphFormatHeaderPound = $F0274; glyphFormatIndentDecrease = $F0275; glyphFormatIndentIncrease = $F0276;
  glyphFormatItalic = $F0277; glyphFormatLineSpacing = $F0278; glyphFormatListBulleted = $F0279;
  glyphFormatListBulletedType = $F027A; glyphFormatListNumbered = $F027B; glyphFormatPaint = $F027C;
  glyphFormatParagraph = $F027D; glyphFormatQuoteClose = $F027E; glyphFormatSize = $F027F;
  glyphFormatStrikethrough = $F0280; glyphFormatStrikethroughVariant = $F0281; glyphFormatSubscript = $F0282;
  glyphFormatSuperscript = $F0283; glyphFormatText = $F0284; glyphFormatTextdirectionLToR = $F0285;
  glyphFormatTextdirectionRToL = $F0286; glyphFormatUnderline = $F0287; glyphFormatWrapInline = $F0288;
  glyphFormatWrapSquare = $F0289; glyphFormatWrapTight = $F028A; glyphFormatWrapTopBottom = $F028B;
  glyphForum = $F028C; glyphForward = $F028D; glyphBowl = $F028E;
  glyphFridgeOutline = $F028F; glyphFridge = $F0290; glyphFridgeTop = $F0291;
  glyphFridgeBottom = $F0292; glyphFullscreen = $F0293; glyphFullscreenExit = $F0294;
  glyphFunction = $F0295; glyphGamepad = $F0296; glyphGamepadVariant = $F0297;
  glyphGasStation = $F0298; glyphGate = $F0299; glyphGauge = $F029A;
  glyphGavel = $F029B; glyphGenderFemale = $F029C; glyphGenderMale = $F029D;
  glyphGenderMaleFemale = $F029E; glyphGenderTransgender = $F029F; glyphGhost = $F02A0;
  glyphGiftOutline = $F02A1; glyphGit = $F02A2; glyphCardAccountDetailsStar = $F02A3;
  glyphGithub = $F02A4; glyphGlassFlute = $F02A5; glyphGlassMug = $F02A6;
  glyphGlassStange = $F02A7; glyphGlassTulip = $F02A8; glyphBowlOutline = $F02A9;
  glyphGlasses = $F02AA; glyphGmail = $F02AB; glyphGnome = $F02AC;
  glyphGoogle = $F02AD; glyphGoogleCardboard = $F02AE; glyphGoogleChrome = $F02AF;
  glyphGoogleCircles = $F02B0; glyphGoogleCirclesCommunities = $F02B1; glyphGoogleCirclesExtended = $F02B2;
  glyphGoogleCirclesGroup = $F02B3; glyphGoogleController = $F02B4; glyphGoogleControllerOff = $F02B5;
  glyphGoogleDrive = $F02B6; glyphGoogleEarth = $F02B7; glyphGoogleGlass = $F02B8;
  glyphGoogleNearby = $F02B9; glyphVideoMinusOutline = $F02BA; glyphMicrosoftTeams = $F02BB;
  glyphGooglePlay = $F02BC; glyphGooglePlus = $F02BD; glyphOrderBoolAscending = $F02BE;
  glyphGoogleTranslate = $F02BF; glyphGoogleClassroom = $F02C0; glyphGrid = $F02C1;
  glyphGridOff = $F02C2; glyphGroup = $F02C3; glyphGuitarElectric = $F02C4;
  glyphGuitarPick = $F02C5; glyphGuitarPickOutline = $F02C6; glyphHandPointingRight = $F02C7;
  glyphHanger = $F02C8; glyphGoogleHangouts = $F02C9; glyphHarddisk = $F02CA;
  glyphHeadphones = $F02CB; glyphHeadphonesBox = $F02CC; glyphHeadphonesSettings = $F02CD;
  glyphHeadset = $F02CE; glyphHeadsetDock = $F02CF; glyphHeadsetOff = $F02D0;
  glyphHeart = $F02D1; glyphHeartBox = $F02D2; glyphHeartBoxOutline = $F02D3;
  glyphHeartBroken = $F02D4; glyphHeartOutline = $F02D5; glyphHelp = $F02D6;
  glyphHelpCircle = $F02D7; glyphHexagon = $F02D8; glyphHexagonOutline = $F02D9;
  glyphHistory = $F02DA; glyphHololens = $F02DB; glyphHome = $F02DC;
  glyphHomeModern = $F02DD; glyphHomeVariant = $F02DE; glyphHops = $F02DF;
  glyphHospitalBox = $F02E0; glyphHospitalBuilding = $F02E1; glyphHospitalMarker = $F02E2;
  glyphBed = $F02E3; glyphBowlMixOutline = $F02E4; glyphPot = $F02E5;
  glyphHuman = $F02E6; glyphHumanChild = $F02E7; glyphHumanMaleFemale = $F02E8;
  glyphImage = $F02E9; glyphImageAlbum = $F02EA; glyphImageArea = $F02EB;
  glyphImageAreaClose = $F02EC; glyphImageBroken = $F02ED; glyphImageBrokenVariant = $F02EE;
  glyphImageMultipleOutline = $F02EF; glyphImageFilterBlackWhite = $F02F0; glyphImageFilterCenterFocus = $F02F1;
  glyphImageFilterCenterFocusWeak = $F02F2; glyphImageFilterDrama = $F02F3; glyphImageFilterFrames = $F02F4;
  glyphImageFilterHdr = $F02F5; glyphImageFilterNone = $F02F6; glyphImageFilterTiltShift = $F02F7;
  glyphImageFilterVintage = $F02F8; glyphImageMultiple = $F02F9; glyphImport = $F02FA;
  glyphInboxArrowDown = $F02FB; glyphInformation = $F02FC; glyphInformationOutline = $F02FD;
  glyphInstagram = $F02FE; glyphPotOutline = $F02FF; glyphMicrosoftInternetExplorer = $F0300;
  glyphInvertColors = $F0301; glyphJeepney = $F0302; glyphJira = $F0303;
  glyphJsfiddle = $F0304; glyphKeg = $F0305; glyphKey = $F0306;
  glyphKeyChange = $F0307; glyphKeyMinus = $F0308; glyphKeyPlus = $F0309;
  glyphKeyRemove = $F030A; glyphKeyVariant = $F030B; glyphKeyboard = $F030C;
  glyphKeyboardBackspace = $F030D; glyphKeyboardCaps = $F030E; glyphKeyboardClose = $F030F;
  glyphKeyboardOff = $F0310; glyphKeyboardReturn = $F0311; glyphKeyboardTab = $F0312;
  glyphKeyboardVariant = $F0313; glyphKodi = $F0314; glyphLabel = $F0315;
  glyphLabelOutline = $F0316; glyphLan = $F0317; glyphLanConnect = $F0318;
  glyphLanDisconnect = $F0319; glyphLanPending = $F031A; glyphLanguageCsharp = $F031B;
  glyphLanguageCss3 = $F031C; glyphLanguageHtml5 = $F031D; glyphLanguageJavascript = $F031E;
  glyphLanguagePhp = $F031F; glyphLanguagePython = $F0320; glyphContactlessPaymentCircle = $F0321;
  glyphLaptop = $F0322; glyphMagazineRifle = $F0323; glyphMagazinePistol = $F0324;
  glyphKeyboardTabReverse = $F0325; glyphPotSteamOutline = $F0326; glyphLaunch = $F0327;
  glyphLayers = $F0328; glyphLayersOff = $F0329; glyphLeaf = $F032A;
  glyphLedOff = $F032B; glyphLedOn = $F032C; glyphLedOutline = $F032D;
  glyphLedVariantOff = $F032E; glyphLedVariantOn = $F032F; glyphLedVariantOutline = $F0330;
  glyphLibrary = $F0331; glyphFilmstripBox = $F0332; glyphMusicBoxMultiple = $F0333;
  glyphPlusBoxMultiple = $F0334; glyphLightbulb = $F0335; glyphLightbulbOutline = $F0336;
  glyphLink = $F0337; glyphLinkOff = $F0338; glyphLinkVariant = $F0339;
  glyphLinkVariantOff = $F033A; glyphLinkedin = $F033B; glyphSortReverseVariant = $F033C;
  glyphLinux = $F033D; glyphLock = $F033E; glyphLockOpen = $F033F;
  glyphLockOpenOutline = $F0340; glyphLockOutline = $F0341; glyphLogin = $F0342;
  glyphLogout = $F0343; glyphLooks = $F0344; glyphLoupe = $F0345;
  glyphLumx = $F0346; glyphMagnet = $F0347; glyphMagnetOn = $F0348;
  glyphMagnify = $F0349; glyphMagnifyMinus = $F034A; glyphMagnifyPlus = $F034B;
  glyphPlusCircleMultiple = $F034C; glyphMap = $F034D; glyphMapMarker = $F034E;
  glyphMapMarkerCircle = $F034F; glyphMapMarkerMultiple = $F0350; glyphMapMarkerOff = $F0351;
  glyphMapMarkerRadius = $F0352; glyphMargin = $F0353; glyphLanguageMarkdown = $F0354;
  glyphMarkerCheck = $F0355; glyphGlassCocktail = $F0356; glyphMaterialUi = $F0357;
  glyphMathCompass = $F0358; glyphStackpath = $F0359; glyphMinusCircleMultiple = $F035A;
  glyphMemory = $F035B; glyphMenu = $F035C; glyphMenuDown = $F035D;
  glyphMenuLeft = $F035E; glyphMenuRight = $F035F; glyphMenuUp = $F0360;
  glyphMessage = $F0361; glyphMessageAlert = $F0362; glyphMessageDraw = $F0363;
  glyphMessageImage = $F0364; glyphMessageOutline = $F0365; glyphMessageProcessing = $F0366;
  glyphMessageReply = $F0367; glyphMessageReplyText = $F0368; glyphMessageText = $F0369;
  glyphMessageTextOutline = $F036A; glyphMessageVideo = $F036B; glyphMicrophone = $F036C;
  glyphMicrophoneOff = $F036D; glyphMicrophoneOutline = $F036E; glyphMicrophoneSettings = $F036F;
  glyphMicrophoneVariant = $F0370; glyphMicrophoneVariantOff = $F0371; glyphMicrosoft = $F0372;
  glyphMinecraft = $F0373; glyphMinus = $F0374; glyphMinusBox = $F0375;
  glyphMinusCircle = $F0376; glyphMinusCircleOutline = $F0377; glyphMinusNetwork = $F0378;
  glyphMonitor = $F0379; glyphMonitorMultiple = $F037A; glyphMore = $F037B;
  glyphMotorbike = $F037C; glyphMouse = $F037D; glyphMouseOff = $F037E;
  glyphMouseVariant = $F037F; glyphMouseVariantOff = $F0380; glyphMovie = $F0381;
  glyphMultiplication = $F0382; glyphMultiplicationBox = $F0383; glyphMusicBox = $F0384;
  glyphMusicBoxOutline = $F0385; glyphMusicCircle = $F0386; glyphMusicNote = $F0387;
  glyphMusicNoteHalf = $F0389; glyphMusicNoteOff = $F038A; glyphMusicNoteQuarter = $F038B;
  glyphMusicNoteSixteenth = $F038C; glyphMusicNoteWhole = $F038D; glyphNature = $F038E;
  glyphNaturePeople = $F038F; glyphNavigation = $F0390; glyphNeedle = $F0391;
  glyphSmokeDetector = $F0392; glyphThermostat = $F0393; glyphNewBox = $F0394;
  glyphNewspaper = $F0395; glyphNfc = $F0396; glyphNfcTap = $F0397;
  glyphNfcVariant = $F0398; glyphNodejs = $F0399; glyphNote = $F039A;
  glyphNoteOutline = $F039B; glyphNotePlus = $F039C; glyphNotePlusOutline = $F039D;
  glyphNoteText = $F039E; glyphNotificationClearAll = $F039F; glyphNumeric = $F03A0;
  glyphNumeric0Box = $F03A1; glyphNumeric0BoxMultipleOutline = $F03A2; glyphNumeric0BoxOutline = $F03A3;
  glyphNumeric1Box = $F03A4; glyphNumeric1BoxMultipleOutline = $F03A5; glyphNumeric1BoxOutline = $F03A6;
  glyphNumeric2Box = $F03A7; glyphNumeric2BoxMultipleOutline = $F03A8; glyphNumeric2BoxOutline = $F03A9;
  glyphNumeric3Box = $F03AA; glyphNumeric3BoxMultipleOutline = $F03AB; glyphNumeric3BoxOutline = $F03AC;
  glyphNumeric4Box = $F03AD; glyphNumeric4BoxOutline = $F03AE; glyphNumeric5BoxMultipleOutline = $F03AF;
  glyphNumeric5BoxOutline = $F03B0; glyphNumeric5Box = $F03B1; glyphNumeric4BoxMultipleOutline = $F03B2;
  glyphNumeric6Box = $F03B3; glyphNumeric6BoxMultipleOutline = $F03B4; glyphNumeric6BoxOutline = $F03B5;
  glyphNumeric7Box = $F03B6; glyphNumeric7BoxMultipleOutline = $F03B7; glyphNumeric7BoxOutline = $F03B8;
  glyphNumeric8Box = $F03B9; glyphNumeric8BoxMultipleOutline = $F03BA; glyphNumeric8BoxOutline = $F03BB;
  glyphNumeric9Box = $F03BC; glyphNumeric9BoxMultipleOutline = $F03BD; glyphNumeric9BoxOutline = $F03BE;
  glyphNumeric9PlusBox = $F03BF; glyphNumeric9PlusBoxMultipleOutline = $F03C0; glyphNumeric9PlusBoxOutline = $F03C1;
  glyphNutrition = $F03C2; glyphOctagon = $F03C3; glyphOctagonOutline = $F03C4;
  glyphOdnoklassniki = $F03C5; glyphMicrosoftOffice = $F03C6; glyphOil = $F03C7;
  glyphCoolantTemperature = $F03C8; glyphOmega = $F03C9; glyphMicrosoftOnedrive = $F03CA;
  glyphOpenInApp = $F03CB; glyphOpenInNew = $F03CC; glyphOpenid = $F03CD;
  glyphOpera = $F03CE; glyphOrnament = $F03CF; glyphOrnamentVariant = $F03D0;
  glyphInboxArrowUp = $F03D1; glyphOwl = $F03D2; glyphPackage = $F03D3;
  glyphPackageDown = $F03D4; glyphPackageUp = $F03D5; glyphPackageVariant = $F03D6;
  glyphPackageVariantClosed = $F03D7; glyphPalette = $F03D8; glyphPaletteAdvanced = $F03D9;
  glyphPanda = $F03DA; glyphPandora = $F03DB; glyphPanorama = $F03DC;
  glyphPanoramaFisheye = $F03DD; glyphPanoramaHorizontalOutline = $F03DE; glyphPanoramaVerticalOutline = $F03DF;
  glyphPanoramaWideAngleOutline = $F03E0; glyphPaperCutVertical = $F03E1; glyphPaperclip = $F03E2;
  glyphParking = $F03E3; glyphPause = $F03E4; glyphPauseCircle = $F03E5;
  glyphPauseCircleOutline = $F03E6; glyphPauseOctagon = $F03E7; glyphPauseOctagonOutline = $F03E8;
  glyphPaw = $F03E9; glyphPen = $F03EA; glyphPencil = $F03EB;
  glyphPencilBox = $F03EC; glyphPencilBoxOutline = $F03ED; glyphPencilLock = $F03EE;
  glyphPencilOff = $F03EF; glyphPercent = $F03F0; glyphMortarPestlePlus = $F03F1;
  glyphPhone = $F03F2; glyphPhoneBluetooth = $F03F3; glyphPhoneForward = $F03F4;
  glyphPhoneHangup = $F03F5; glyphPhoneInTalk = $F03F6; glyphPhoneIncoming = $F03F7;
  glyphPhoneLock = $F03F8; glyphPhoneLog = $F03F9; glyphPhoneMissed = $F03FA;
  glyphPhoneOutgoing = $F03FB; glyphPhonePaused = $F03FC; glyphPhoneSettings = $F03FD;
  glyphPhoneVoip = $F03FE; glyphPi = $F03FF; glyphPiBox = $F0400;
  glyphPig = $F0401; glyphPill = $F0402; glyphPin = $F0403;
  glyphPinOff = $F0404; glyphPineTree = $F0405; glyphPineTreeBox = $F0406;
  glyphPinterest = $F0407; glyphContactlessPaymentCircleOutline = $F0408; glyphPizza = $F0409;
  glyphPlay = $F040A; glyphPlayBoxOutline = $F040B; glyphPlayCircle = $F040C;
  glyphPlayCircleOutline = $F040D; glyphPlayPause = $F040E; glyphPlayProtectedContent = $F040F;
  glyphPlaylistMinus = $F0410; glyphPlaylistPlay = $F0411; glyphPlaylistPlus = $F0412;
  glyphPlaylistRemove = $F0413; glyphSonyPlaystation = $F0414; glyphPlus = $F0415;
  glyphPlusBox = $F0416; glyphPlusCircle = $F0417; glyphPlusCircleMultipleOutline = $F0418;
  glyphPlusCircleOutline = $F0419; glyphPlusNetwork = $F041A; glyphSledding = $F041B;
  glyphWallSconceFlatVariant = $F041C; glyphPokeball = $F041D; glyphPolaroid = $F041E;
  glyphPoll = $F041F; glyphAccountEye = $F0420; glyphPolymer = $F0421;
  glyphPopcorn = $F0422; glyphPound = $F0423; glyphPoundBox = $F0424;
  glyphPower = $F0425; glyphPowerSettings = $F0426; glyphPowerSocket = $F0427;
  glyphPresentation = $F0428; glyphPresentationPlay = $F0429; glyphPrinter = $F042A;
  glyphPrinter3d = $F042B; glyphPrinterAlert = $F042C; glyphProfessionalHexagon = $F042D;
  glyphProjector = $F042E; glyphProjectorScreen = $F042F; glyphPulse = $F0430;
  glyphPuzzle = $F0431; glyphQrcode = $F0432; glyphQrcodeScan = $F0433;
  glyphQuadcopter = $F0434; glyphQualityHigh = $F0435; glyphBookMultipleOutline = $F0436;
  glyphRadar = $F0437; glyphRadiator = $F0438; glyphRadio = $F0439;
  glyphRadioHandheld = $F043A; glyphRadioTower = $F043B; glyphRadioactive = $F043C;
  glyphRadioboxMarked = $F043E; glyphRaspberryPi = $F043F; glyphRayEnd = $F0440;
  glyphRayEndArrow = $F0441; glyphRayStart = $F0442; glyphRayStartArrow = $F0443;
  glyphRayStartEnd = $F0444; glyphRayVertex = $F0445; glyphLastpass = $F0446;
  glyphRead = $F0447; glyphYoutubeTv = $F0448; glyphReceipt = $F0449;
  glyphRecord = $F044A; glyphRecordRec = $F044B; glyphRecycle = $F044C;
  glyphReddit = $F044D; glyphRedo = $F044E; glyphRedoVariant = $F044F;
  glyphRefresh = $F0450; glyphRegex = $F0451; glyphRelativeScale = $F0452;
  glyphReload = $F0453; glyphRemote = $F0454; glyphRenameBox = $F0455;
  glyphRepeat = $F0456; glyphRepeatOff = $F0457; glyphRepeatOnce = $F0458;
  glyphReplay = $F0459; glyphReply = $F045A; glyphReplyAll = $F045B;
  glyphReproduction = $F045C; glyphResizeBottomRight = $F045D; glyphResponsive = $F045E;
  glyphRewind = $F045F; glyphRibbon = $F0460; glyphRoad = $F0461;
  glyphRoadVariant = $F0462; glyphRocket = $F0463; glyphRotate3dVariant = $F0464;
  glyphRotateLeft = $F0465; glyphRotateLeftVariant = $F0466; glyphRotateRight = $F0467;
  glyphRotateRightVariant = $F0468; glyphRouterWireless = $F0469; glyphRoutes = $F046A;
  glyphRss = $F046B; glyphRssBox = $F046C; glyphRuler = $F046D;
  glyphRunFast = $F046E; glyphSale = $F046F; glyphSatellite = $F0470;
  glyphSatelliteVariant = $F0471; glyphScale = $F0472; glyphScaleBathroom = $F0473;
  glyphSchool = $F0474; glyphScreenRotation = $F0475; glyphScrewdriver = $F0476;
  glyphScriptOutline = $F0477; glyphScreenRotationLock = $F0478; glyphSd = $F0479;
  glyphSeal = $F047A; glyphSeatFlat = $F047B; glyphSeatFlatAngled = $F047C;
  glyphSeatIndividualSuite = $F047D; glyphSeatLegroomExtra = $F047E; glyphSeatLegroomNormal = $F047F;
  glyphSeatLegroomReduced = $F0480; glyphSeatReclineExtra = $F0481; glyphSeatReclineNormal = $F0482;
  glyphSecurity = $F0483; glyphSecurityNetwork = $F0484; glyphSelect = $F0485;
  glyphSelectAll = $F0486; glyphSelectInverse = $F0487; glyphSelectOff = $F0488;
  glyphSelection = $F0489; glyphSend = $F048A; glyphServer = $F048B;
  glyphServerMinus = $F048C; glyphServerNetwork = $F048D; glyphServerNetworkOff = $F048E;
  glyphServerOff = $F048F; glyphServerPlus = $F0490; glyphServerRemove = $F0491;
  glyphServerSecurity = $F0492; glyphCog = $F0493; glyphCogBox = $F0494;
  glyphShapePlus = $F0495; glyphShare = $F0496; glyphShareVariant = $F0497;
  glyphShield = $F0498; glyphShieldOutline = $F0499; glyphShopping = $F049A;
  glyphShoppingMusic = $F049B; glyphShredder = $F049C; glyphShuffle = $F049D;
  glyphShuffleDisabled = $F049E; glyphShuffleVariant = $F049F; glyphSigma = $F04A0;
  glyphSignCaution = $F04A1; glyphSignal = $F04A2; glyphSilverware = $F04A3;
  glyphSilverwareFork = $F04A4; glyphSilverwareSpoon = $F04A5; glyphSilverwareVariant = $F04A6;
  glyphSim = $F04A7; glyphSimAlert = $F04A8; glyphSimOff = $F04A9;
  glyphSitemap = $F04AA; glyphSkipBackward = $F04AB; glyphSkipForward = $F04AC;
  glyphSkipNext = $F04AD; glyphSkipPrevious = $F04AE; glyphSkype = $F04AF;
  glyphSkypeBusiness = $F04B0; glyphSlack = $F04B1; glyphSleep = $F04B2;
  glyphSleepOff = $F04B3; glyphSmoking = $F04B4; glyphSmokingOff = $F04B5;
  glyphSnapchat = $F04B6; glyphSnowman = $F04B7; glyphSoccer = $F04B8;
  glyphSofa = $F04B9; glyphSort = $F04BA; glyphSortAlphabeticalVariant = $F04BB;
  glyphSortAscending = $F04BC; glyphSortDescending = $F04BD; glyphSortNumericVariant = $F04BE;
  glyphSortVariant = $F04BF; glyphSoundcloud = $F04C0; glyphSourceFork = $F04C1;
  glyphSourcePull = $F04C2; glyphSpeaker = $F04C3; glyphSpeakerOff = $F04C4;
  glyphSpeedometer = $F04C5; glyphSpellcheck = $F04C6; glyphSpotify = $F04C7;
  glyphSpotlight = $F04C8; glyphSpotlightBeam = $F04C9; glyphBookRemoveMultipleOutline = $F04CA;
  glyphAccountSwitchOutline = $F04CB; glyphStackOverflow = $F04CC; glyphStairs = $F04CD;
  glyphStar = $F04CE; glyphStarCircle = $F04CF; glyphStarHalfFull = $F04D0;
  glyphStarOff = $F04D1; glyphStarOutline = $F04D2; glyphSteam = $F04D3;
  glyphSteering = $F04D4; glyphStepBackward = $F04D5; glyphStepBackward2 = $F04D6;
  glyphStepForward = $F04D7; glyphStepForward2 = $F04D8; glyphStethoscope = $F04D9;
  glyphStocking = $F04DA; glyphStop = $F04DB; glyphStore = $F04DC;
  glyphStore24Hour = $F04DD; glyphStove = $F04DE; glyphSubwayVariant = $F04DF;
  glyphSunglasses = $F04E0; glyphSwapHorizontal = $F04E1; glyphSwapVertical = $F04E2;
  glyphSwim = $F04E3; glyphSwitch = $F04E4; glyphSword = $F04E5;
  glyphSync = $F04E6; glyphSyncAlert = $F04E7; glyphSyncOff = $F04E8;
  glyphTab = $F04E9; glyphTabUnselected = $F04EA; glyphTable = $F04EB;
  glyphTableColumnPlusAfter = $F04EC; glyphTableColumnPlusBefore = $F04ED; glyphTableColumnRemove = $F04EE;
  glyphTableColumnWidth = $F04EF; glyphTableEdit = $F04F0; glyphTableLarge = $F04F1;
  glyphTableRowHeight = $F04F2; glyphTableRowPlusAfter = $F04F3; glyphTableRowPlusBefore = $F04F4;
  glyphTableRowRemove = $F04F5; glyphTablet = $F04F6; glyphTabletAndroid = $F04F7;
  glyphTangram = $F04F8; glyphTag = $F04F9; glyphTagFaces = $F04FA;
  glyphTagMultiple = $F04FB; glyphTagOutline = $F04FC; glyphTagTextOutline = $F04FD;
  glyphTarget = $F04FE; glyphTaxi = $F04FF; glyphTeamviewer = $F0500;
  glyphSkateboarding = $F0501; glyphTelevision = $F0502; glyphTelevisionGuide = $F0503;
  glyphTemperatureCelsius = $F0504; glyphTemperatureFahrenheit = $F0505; glyphTemperatureKelvin = $F0506;
  glyphTennisBall = $F0507; glyphTent = $F0508; glyphTextToSpeech = $F050A;
  glyphTextToSpeechOff = $F050B; glyphTexture = $F050C; glyphTheater = $F050D;
  glyphThemeLightDark = $F050E; glyphThermometer = $F050F; glyphThermometerLines = $F0510;
  glyphThumbDown = $F0511; glyphThumbDownOutline = $F0512; glyphThumbUp = $F0513;
  glyphThumbUpOutline = $F0514; glyphThumbsUpDown = $F0515; glyphTicket = $F0516;
  glyphTicketAccount = $F0517; glyphTicketConfirmation = $F0518; glyphTie = $F0519;
  glyphTimelapse = $F051A; glyphTimerOutline = $F051B; glyphTimer10 = $F051C;
  glyphTimer3 = $F051D; glyphTimerOffOutline = $F051E; glyphTimerSand = $F051F;
  glyphTimetable = $F0520; glyphToggleSwitch = $F0521; glyphToggleSwitchOff = $F0522;
  glyphTooltip = $F0523; glyphTooltipEdit = $F0524; glyphTooltipImage = $F0525;
  glyphTooltipOutline = $F0526; glyphTooltipPlusOutline = $F0527; glyphTooltipText = $F0528;
  glyphToothOutline = $F0529; glyphCloudRefresh = $F052A; glyphTrafficLight = $F052B;
  glyphTrain = $F052C; glyphTram = $F052D; glyphTranscribe = $F052E;
  glyphTranscribeClose = $F052F; glyphTransferRight = $F0530; glyphTree = $F0531;
  glyphTrello = $F0532; glyphTrendingDown = $F0533; glyphTrendingNeutral = $F0534;
  glyphTrendingUp = $F0535; glyphTriangle = $F0536; glyphTriangleOutline = $F0537;
  glyphTrophy = $F0538; glyphTrophyAward = $F0539; glyphTrophyOutline = $F053A;
  glyphTrophyVariant = $F053B; glyphTrophyVariantOutline = $F053C; glyphTruck = $F053D;
  glyphTruckDelivery = $F053E; glyphTshirtCrewOutline = $F053F; glyphTshirtVOutline = $F0540;
  glyphFileRefreshOutline = $F0541; glyphFolderRefreshOutline = $F0542; glyphTwitch = $F0543;
  glyphTwitter = $F0544; glyphOrderNumericAscending = $F0545; glyphOrderNumericDescending = $F0546;
  glyphRepeatVariant = $F0547; glyphUbuntu = $F0548; glyphUmbraco = $F0549;
  glyphUmbrella = $F054A; glyphUmbrellaOutline = $F054B; glyphUndo = $F054C;
  glyphUndoVariant = $F054D; glyphUnfoldLessHorizontal = $F054E; glyphUnfoldMoreHorizontal = $F054F;
  glyphUngroup = $F0550; glyphWebRemove = $F0551; glyphUpload = $F0552;
  glyphUsb = $F0553; glyphVectorArrangeAbove = $F0554; glyphVectorArrangeBelow = $F0555;
  glyphVectorCircle = $F0556; glyphVectorCircleVariant = $F0557; glyphVectorCombine = $F0558;
  glyphVectorCurve = $F0559; glyphVectorDifference = $F055A; glyphVectorDifferenceAb = $F055B;
  glyphVectorDifferenceBa = $F055C; glyphVectorIntersection = $F055D; glyphVectorLine = $F055E;
  glyphVectorPoint = $F055F; glyphVectorPolygon = $F0560; glyphVectorPolyline = $F0561;
  glyphVectorSelection = $F0562; glyphVectorTriangle = $F0563; glyphVectorUnion = $F0564;
  glyphShieldCheck = $F0565; glyphVibrate = $F0566; glyphVideo = $F0567;
  glyphVideoOff = $F0568; glyphVideoSwitch = $F0569; glyphViewAgenda = $F056A;
  glyphViewArray = $F056B; glyphViewCarousel = $F056C; glyphViewColumn = $F056D;
  glyphViewDashboard = $F056E; glyphViewDay = $F056F; glyphViewGrid = $F0570;
  glyphViewHeadline = $F0571; glyphViewList = $F0572; glyphViewModule = $F0573;
  glyphViewQuilt = $F0574; glyphViewStream = $F0575; glyphViewWeek = $F0576;
  glyphVimeo = $F0577; glyphBuffet = $F0578; glyphHandsPray = $F0579;
  glyphCreditCardWirelessOff = $F057A; glyphCreditCardWirelessOffOutline = $F057B; glyphVlc = $F057C;
  glyphVoicemail = $F057D; glyphVolumeHigh = $F057E; glyphVolumeLow = $F057F;
  glyphVolumeMedium = $F0580; glyphVolumeOff = $F0581; glyphVpn = $F0582;
  glyphWalk = $F0583; glyphWallet = $F0584; glyphWalletGiftcard = $F0585;
  glyphWalletMembership = $F0586; glyphWalletTravel = $F0587; glyphWan = $F0588;
  glyphWatch = $F0589; glyphWatchExport = $F058A; glyphWatchImport = $F058B;
  glyphWater = $F058C; glyphWaterOff = $F058D; glyphWaterPercent = $F058E;
  glyphWaterPump = $F058F; glyphWeatherCloudy = $F0590; glyphWeatherFog = $F0591;
  glyphWeatherHail = $F0592; glyphWeatherLightning = $F0593; glyphWeatherNight = $F0594;
  glyphWeatherPartlyCloudy = $F0595; glyphWeatherPouring = $F0596; glyphWeatherRainy = $F0597;
  glyphWeatherSnowy = $F0598; glyphWeatherSunny = $F0599; glyphWeatherSunset = $F059A;
  glyphWeatherSunsetDown = $F059B; glyphWeatherSunsetUp = $F059C; glyphWeatherWindy = $F059D;
  glyphWeatherWindyVariant = $F059E; glyphWeb = $F059F; glyphWebcam = $F05A0;
  glyphWeight = $F05A1; glyphWeightKilogram = $F05A2; glyphWhatsapp = $F05A3;
  glyphWheelchairAccessibility = $F05A4; glyphWhiteBalanceAuto = $F05A5; glyphWhiteBalanceIncandescent = $F05A6;
  glyphWhiteBalanceIridescent = $F05A7; glyphWhiteBalanceSunny = $F05A8; glyphWifi = $F05A9;
  glyphWifiOff = $F05AA; glyphNintendoWii = $F05AB; glyphWikipedia = $F05AC;
  glyphWindowClose = $F05AD; glyphWindowClosed = $F05AE; glyphWindowMaximize = $F05AF;
  glyphWindowMinimize = $F05B0; glyphWindowOpen = $F05B1; glyphWindowRestore = $F05B2;
  glyphMicrosoftWindows = $F05B3; glyphWordpress = $F05B4; glyphAccountHardHat = $F05B5;
  glyphWrap = $F05B6; glyphWrench = $F05B7; glyphContactsOutline = $F05B8;
  glyphMicrosoftXbox = $F05B9; glyphMicrosoftXboxController = $F05BA; glyphMicrosoftXboxControllerOff = $F05BB;
  glyphTableFurniture = $F05BC; glyphSortAlphabeticalAscending = $F05BD; glyphFirewire = $F05BE;
  glyphSortAlphabeticalDescending = $F05BF; glyphXml = $F05C0; glyphYeast = $F05C1;
  glyphDatabaseRefresh = $F05C2; glyphYoutube = $F05C3; glyphZipBox = $F05C4;
  glyphSurroundSound = $F05C5; glyphVectorRectangle = $F05C6; glyphPlaylistCheck = $F05C7;
  glyphFormatLineStyle = $F05C8; glyphFormatLineWeight = $F05C9; glyphTranslate = $F05CA;
  glyphAccountVoice = $F05CB; glyphOpacity = $F05CC; glyphNearMe = $F05CD;
  glyphClockAlertOutline = $F05CE; glyphHumanPregnant = $F05CF; glyphStickerCircleOutline = $F05D0;
  glyphScaleBalance = $F05D1; glyphCardAccountDetails = $F05D2; glyphAccountMultipleMinus = $F05D3;
  glyphAirplaneLanding = $F05D4; glyphAirplaneTakeoff = $F05D5; glyphAlertCircleOutline = $F05D6;
  glyphAltimeter = $F05D7; glyphAnimation = $F05D8; glyphBookMinus = $F05D9;
  glyphBookOpenPageVariant = $F05DA; glyphBookPlus = $F05DB; glyphBoombox = $F05DC;
  glyphBullseye = $F05DD; glyphCommentRemove = $F05DE; glyphCameraOff = $F05DF;
  glyphCheckCircle = $F05E0; glyphCheckCircleOutline = $F05E1; glyphCandle = $F05E2;
  glyphChartBubble = $F05E3; glyphCreditCardOffOutline = $F05E4; glyphCupOff = $F05E5;
  glyphCopyright = $F05E6; glyphCursorText = $F05E7; glyphDeleteForever = $F05E8;
  glyphDeleteSweep = $F05E9; glyphDiceD20Outline = $F05EA; glyphDiceD4Outline = $F05EB;
  glyphDiceD8Outline = $F05EC; glyphDiceD6Outline = $F05ED; glyphDisc = $F05EE;
  glyphEmailOpenOutline = $F05EF; glyphEmailVariant = $F05F0; glyphEvStation = $F05F1;
  glyphFoodForkDrink = $F05F2; glyphFoodOff = $F05F3; glyphFormatTitle = $F05F4;
  glyphGoogleMaps = $F05F5; glyphHeartPulse = $F05F6; glyphHighway = $F05F7;
  glyphHomeMapMarker = $F05F8; glyphIncognito = $F05F9; glyphKettle = $F05FA;
  glyphLockPlus = $F05FB; glyphLogoutVariant = $F05FD; glyphMusicNoteBluetooth = $F05FE;
  glyphMusicNoteBluetoothOff = $F05FF; glyphPageFirst = $F0600; glyphPageLast = $F0601;
  glyphPhoneClassic = $F0602; glyphPriorityHigh = $F0603; glyphPriorityLow = $F0604;
  glyphQqchat = $F0605; glyphPool = $F0606; glyphRoundedCorner = $F0607;
  glyphRowing = $F0608; glyphSaxophone = $F0609; glyphSignalVariant = $F060A;
  glyphStackExchange = $F060B; glyphSubdirectoryArrowLeft = $F060C; glyphSubdirectoryArrowRight = $F060D;
  glyphFormTextbox = $F060E; glyphViolin = $F060F; glyphMicrosoftVisualStudio = $F0610;
  glyphWechat = $F0611; glyphWatermark = $F0612; glyphFileHidden = $F0613;
  glyphApplicationOutline = $F0614; glyphArrowCollapse = $F0615; glyphArrowExpand = $F0616;
  glyphBowlMix = $F0617; glyphBridge = $F0618; glyphApplicationEditOutline = $F0619;
  glyphChip = $F061A; glyphContentSaveSettings = $F061B; glyphDialpad = $F061C;
  glyphBookAlphabet = $F061D; glyphFormatHorizontalAlignCenter = $F061E; glyphFormatHorizontalAlignLeft = $F061F;
  glyphFormatHorizontalAlignRight = $F0620; glyphFormatVerticalAlignBottom = $F0621; glyphFormatVerticalAlignCenter = $F0622;
  glyphFormatVerticalAlignTop = $F0623; glyphLineScan = $F0624; glyphHelpCircleOutline = $F0625;
  glyphCodeJson = $F0626; glyphLambda = $F0627; glyphMatrix = $F0628;
  glyphMeteor = $F0629; glyphCloseCircleMultiple = $F062A; glyphSigmaLower = $F062B;
  glyphSourceBranch = $F062C; glyphSourceMerge = $F062D; glyphTune = $F062E;
  glyphWebhook = $F062F; glyphAccountSettings = $F0630; glyphAccountDetails = $F0631;
  glyphAppleKeyboardCaps = $F0632; glyphAppleKeyboardCommand = $F0633; glyphAppleKeyboardControl = $F0634;
  glyphAppleKeyboardOption = $F0635; glyphAppleKeyboardShift = $F0636; glyphBoxShadow = $F0637;
  glyphCards = $F0638; glyphCardsOutline = $F0639; glyphCardsPlayingOutline = $F063A;
  glyphCheckboxMultipleBlankCircle = $F063B; glyphCheckboxMultipleBlankCircleOutline = $F063C; glyphCheckboxMultipleMarkedCircle = $F063D;
  glyphCheckboxMultipleMarkedCircleOutline = $F063E; glyphCloudSync = $F063F; glyphCollage = $F0640;
  glyphDirectionsFork = $F0641; glyphEraserVariant = $F0642; glyphFaceMan = $F0643;
  glyphFaceManProfile = $F0644; glyphFileTree = $F0645; glyphFormatAnnotationPlus = $F0646;
  glyphGasCylinder = $F0647; glyphGreasePencil = $F0648; glyphHumanFemale = $F0649;
  glyphHumanGreetingVariant = $F064A; glyphHumanHandsdown = $F064B; glyphHumanHandsup = $F064C;
  glyphHumanMale = $F064D; glyphInformationVariant = $F064E; glyphLeadPencil = $F064F;
  glyphMapMarkerMinus = $F0650; glyphMapMarkerPlus = $F0651; glyphMarker = $F0652;
  glyphMessagePlus = $F0653; glyphMicroscope = $F0654; glyphMoveResize = $F0655;
  glyphMoveResizeVariant = $F0656; glyphPawOff = $F0657; glyphPhoneMinus = $F0658;
  glyphPhonePlus = $F0659; glyphPotSteam = $F065A; glyphPotMix = $F065B;
  glyphSerialPort = $F065C; glyphShapeCirclePlus = $F065D; glyphShapePolygonPlus = $F065E;
  glyphShapeRectanglePlus = $F065F; glyphShapeSquarePlus = $F0660; glyphSkipNextCircle = $F0661;
  glyphSkipNextCircleOutline = $F0662; glyphSkipPreviousCircle = $F0663; glyphSkipPreviousCircleOutline = $F0664;
  glyphSpray = $F0665; glyphStopCircle = $F0666; glyphStopCircleOutline = $F0667;
  glyphTestTube = $F0668; glyphTextShadow = $F0669; glyphTuneVertical = $F066A;
  glyphCartOff = $F066B; glyphChartGantt = $F066C; glyphChartScatterPlotHexbin = $F066D;
  glyphChartTimeline = $F066E; glyphDiscord = $F066F; glyphFileRestore = $F0670;
  glyphLanguageC = $F0671; glyphLanguageCpp = $F0672; glyphLanguageXaml = $F0673;
  glyphCreation = $F0674; glyphApplicationCog = $F0675; glyphCreditCardPlusOutline = $F0676;
  glyphPotMixOutline = $F0677; glyphBowTie = $F0678; glyphCalendarRange = $F0679;
  glyphCurrencyUsdOff = $F067A; glyphFlashRedEye = $F067B; glyphOar = $F067C;
  glyphPiano = $F067D; glyphWeatherLightningRainy = $F067E; glyphWeatherSnowyRainy = $F067F;
  glyphYinYang = $F0680; glyphTowerBeach = $F0681; glyphTowerFire = $F0682;
  glyphDeleteCircle = $F0683; glyphDna = $F0684; glyphHamburger = $F0685;
  glyphGondola = $F0686; glyphInbox = $F0687; glyphReorderHorizontal = $F0688;
  glyphReorderVertical = $F0689; glyphShieldHome = $F068A; glyphTagHeart = $F068B;
  glyphSkull = $F068C; glyphSolid = $F068D; glyphAlarmSnooze = $F068E;
  glyphBabyCarriage = $F068F; glyphBeakerOutline = $F0690; glyphBomb = $F0691;
  glyphCalendarQuestion = $F0692; glyphCameraBurst = $F0693; glyphCodeTagsCheck = $F0694;
  glyphCircleMultipleOutline = $F0695; glyphCropRotate = $F0696; glyphDeveloperBoard = $F0697;
  glyphPianoOff = $F0698; glyphSkateOff = $F0699; glyphMessageStar = $F069A;
  glyphEmoticonDeadOutline = $F069B; glyphEmoticonExcitedOutline = $F069C; glyphFolderStar = $F069D;
  glyphFormatColorText = $F069E; glyphFormatSection = $F069F; glyphGradientVertical = $F06A0;
  glyphHomeOutline = $F06A1; glyphMessageBulleted = $F06A2; glyphMessageBulletedOff = $F06A3;
  glyphNuke = $F06A4; glyphPowerPlug = $F06A5; glyphPowerPlugOff = $F06A6;
  glyphPublish = $F06A7; glyphCreditCardMarker = $F06A8; glyphRobot = $F06A9;
  glyphFormatRotate90 = $F06AA; glyphScanner = $F06AB; glyphSubway = $F06AC;
  glyphTimerSandEmpty = $F06AD; glyphTransitTransfer = $F06AE; glyphUnity = $F06AF;
  glyphUpdate = $F06B0; glyphWatchVibrate = $F06B1; glyphAngular = $F06B2;
  glyphDolby = $F06B3; glyphEmby = $F06B4; glyphLamp = $F06B5;
  glyphMenuDownOutline = $F06B6; glyphMenuUpOutline = $F06B7; glyphNoteMultiple = $F06B8;
  glyphNoteMultipleOutline = $F06B9; glyphPlex = $F06BA; glyphShieldAirplane = $F06BB;
  glyphAccountEdit = $F06BC; glyphAlertDecagram = $F06BD; glyphAllInclusive = $F06BE;
  glyphAngularjs = $F06BF; glyphArrowDownBox = $F06C0; glyphArrowLeftBox = $F06C1;
  glyphArrowRightBox = $F06C2; glyphArrowUpBox = $F06C3; glyphAsterisk = $F06C4;
  glyphBombOff = $F06C5; glyphBootstrap = $F06C6; glyphCardsVariant = $F06C7;
  glyphClipboardFlow = $F06C8; glyphCloseOutline = $F06C9; glyphCoffeeOutline = $F06CA;
  glyphContacts = $F06CB; glyphDeleteEmpty = $F06CC; glyphEarthBox = $F06CD;
  glyphEarthBoxOff = $F06CE; glyphEmailAlert = $F06CF; glyphEyeOutline = $F06D0;
  glyphEyeOffOutline = $F06D1; glyphFastForwardOutline = $F06D2; glyphFeather = $F06D3;
  glyphFindReplace = $F06D4; glyphFlashOutline = $F06D5; glyphFormatFont = $F06D6;
  glyphFormatPageBreak = $F06D7; glyphFormatPilcrow = $F06D8; glyphGarage = $F06D9;
  glyphGarageOpen = $F06DA; glyphCardAccountDetailsStarOutline = $F06DB; glyphGoogleKeep = $F06DC;
  glyphSnowmobile = $F06DD; glyphHeartHalfFull = $F06DE; glyphHeartHalf = $F06DF;
  glyphHeartHalfOutline = $F06E0; glyphHexagonMultiple = $F06E1; glyphHook = $F06E2;
  glyphHookOff = $F06E3; glyphInfinity = $F06E4; glyphLanguageSwift = $F06E5;
  glyphLanguageTypescript = $F06E6; glyphLaptopOff = $F06E7; glyphLightbulbOn = $F06E8;
  glyphLightbulbOnOutline = $F06E9; glyphLockPattern = $F06EA; glyphFolderZip = $F06EB;
  glyphMagnifyMinusOutline = $F06EC; glyphMagnifyPlusOutline = $F06ED; glyphMailbox = $F06EE;
  glyphMedicalBag = $F06EF; glyphMessageSettings = $F06F0; glyphMessageCog = $F06F1;
  glyphMinusBoxOutline = $F06F2; glyphNetwork = $F06F3; glyphDownloadNetwork = $F06F4;
  glyphHelpNetwork = $F06F5; glyphUploadNetwork = $F06F6; glyphNpm = $F06F7;
  glyphNut = $F06F8; glyphOctagram = $F06F9; glyphPageLayoutBody = $F06FA;
  glyphPageLayoutFooter = $F06FB; glyphPageLayoutHeader = $F06FC; glyphPageLayoutSidebarLeft = $F06FD;
  glyphPageLayoutSidebarRight = $F06FE; glyphPencilCircle = $F06FF; glyphPentagonOutline = $F0700;
  glyphPentagon = $F0701; glyphPillar = $F0702; glyphPistol = $F0703;
  glyphPlusBoxOutline = $F0704; glyphPlusOutline = $F0705; glyphPrescription = $F0706;
  glyphPrinterSettings = $F0707; glyphReact = $F0708; glyphRestart = $F0709;
  glyphRewindOutline = $F070A; glyphRhombus = $F070B; glyphRhombusOutline = $F070C;
  glyphRobotVacuum = $F070D; glyphRun = $F070E; glyphSearchWeb = $F070F;
  glyphShovel = $F0710; glyphShovelOff = $F0711; glyphSignal2g = $F0712;
  glyphSignal3g = $F0713; glyphSignal4g = $F0714; glyphSignalHspa = $F0715;
  glyphSignalHspaPlus = $F0716; glyphSnowflake = $F0717; glyphSourceCommit = $F0718;
  glyphSourceCommitEnd = $F0719; glyphSourceCommitEndLocal = $F071A; glyphSourceCommitLocal = $F071B;
  glyphSourceCommitNextLocal = $F071C; glyphSourceCommitStart = $F071D; glyphSourceCommitStartNextLocal = $F071E;
  glyphSpeakerWireless = $F071F; glyphStadiumVariant = $F0720; glyphSvg = $F0721;
  glyphTagPlus = $F0722; glyphTagRemove = $F0723; glyphTicketPercent = $F0724;
  glyphTilde = $F0725; glyphTreasureChest = $F0726; glyphTruckTrailer = $F0727;
  glyphViewParallel = $F0728; glyphViewSequential = $F0729; glyphWashingMachine = $F072A;
  glyphWebpack = $F072B; glyphWidgets = $F072C; glyphNintendoWiiu = $F072D;
  glyphArrowDownBold = $F072E; glyphArrowDownBoldBox = $F072F; glyphArrowDownBoldBoxOutline = $F0730;
  glyphArrowLeftBold = $F0731; glyphArrowLeftBoldBox = $F0732; glyphArrowLeftBoldBoxOutline = $F0733;
  glyphArrowRightBold = $F0734; glyphArrowRightBoldBox = $F0735; glyphArrowRightBoldBoxOutline = $F0736;
  glyphArrowUpBold = $F0737; glyphArrowUpBoldBox = $F0738; glyphArrowUpBoldBoxOutline = $F0739;
  glyphCancel = $F073A; glyphFileAccount = $F073B; glyphGestureDoubleTap = $F073C;
  glyphGestureSwipeDown = $F073D; glyphGestureSwipeLeft = $F073E; glyphGestureSwipeRight = $F073F;
  glyphGestureSwipeUp = $F0740; glyphGestureTap = $F0741; glyphGestureTwoDoubleTap = $F0742;
  glyphGestureTwoTap = $F0743; glyphHumbleBundle = $F0744; glyphKickstarter = $F0745;
  glyphNetflix = $F0746; glyphMicrosoftOnenote = $F0747; glyphWallSconceRound = $F0748;
  glyphFolderRefresh = $F0749; glyphVectorRadius = $F074A; glyphMicrosoftXboxControllerBatteryAlert = $F074B;
  glyphMicrosoftXboxControllerBatteryEmpty = $F074C; glyphMicrosoftXboxControllerBatteryFull = $F074D; glyphMicrosoftXboxControllerBatteryLow = $F074E;
  glyphMicrosoftXboxControllerBatteryMedium = $F074F; glyphMicrosoftXboxControllerBatteryUnknown = $F0750; glyphClipboardPlus = $F0751;
  glyphFilePlus = $F0752; glyphFormatAlignBottom = $F0753; glyphFormatAlignMiddle = $F0754;
  glyphFormatAlignTop = $F0755; glyphFormatListChecks = $F0756; glyphFormatQuoteOpen = $F0757;
  glyphGridLarge = $F0758; glyphHeartOff = $F0759; glyphMusic = $F075A;
  glyphMusicOff = $F075B; glyphTabPlus = $F075C; glyphVolumePlus = $F075D;
  glyphVolumeMinus = $F075E; glyphVolumeMute = $F075F; glyphUnfoldLessVertical = $F0760;
  glyphUnfoldMoreVertical = $F0761; glyphTaco = $F0762; glyphSquareOutline = $F0763;
  glyphSquare = $F0764; glyphAlertOctagram = $F0767; glyphAtom = $F0768;
  glyphCeilingLight = $F0769; glyphChartBarStacked = $F076A; glyphChartLineStacked = $F076B;
  glyphDecagram = $F076C; glyphDecagramOutline = $F076D; glyphDiceMultiple = $F076E;
  glyphDiceD10Outline = $F076F; glyphFolderOpen = $F0770; glyphGuitarAcoustic = $F0771;
  glyphLoading = $F0772; glyphLockReset = $F0773; glyphNinja = $F0774;
  glyphOctagramOutline = $F0775; glyphPencilCircleOutline = $F0776; glyphSelectionOff = $F0777;
  glyphSetAll = $F0778; glyphSetCenter = $F0779; glyphSetCenterRight = $F077A;
  glyphSetLeft = $F077B; glyphSetLeftCenter = $F077C; glyphSetLeftRight = $F077D;
  glyphSetNone = $F077E; glyphSetRight = $F077F; glyphShieldHalfFull = $F0780;
  glyphSignDirection = $F0781; glyphSignText = $F0782; glyphSignalOff = $F0783;
  glyphSquareRoot = $F0784; glyphStickerEmoji = $F0785; glyphSummit = $F0786;
  glyphSwordCross = $F0787; glyphTruckFast = $F0788; glyphWebCheck = $F0789;
  glyphCastOff = $F078A; glyphHelpBox = $F078B; glyphTimerSandFull = $F078C;
  glyphWaves = $F078D; glyphAlarmBell = $F078E; glyphAlarmLight = $F078F;
  glyphVideoSwitchOutline = $F0790; glyphCheckDecagram = $F0791; glyphArrowCollapseDown = $F0792;
  glyphArrowCollapseLeft = $F0793; glyphArrowCollapseRight = $F0794; glyphArrowCollapseUp = $F0795;
  glyphArrowExpandDown = $F0796; glyphArrowExpandLeft = $F0797; glyphArrowExpandRight = $F0798;
  glyphArrowExpandUp = $F0799; glyphBookLock = $F079A; glyphBookLockOpen = $F079B;
  glyphBusArticulatedEnd = $F079C; glyphBusArticulatedFront = $F079D; glyphBusDoubleDecker = $F079E;
  glyphBusSchool = $F079F; glyphBusSide = $F07A0; glyphCameraGopro = $F07A1;
  glyphCameraMeteringCenter = $F07A2; glyphCameraMeteringMatrix = $F07A3; glyphCameraMeteringPartial = $F07A4;
  glyphCameraMeteringSpot = $F07A5; glyphCannabis = $F07A6; glyphCarConvertible = $F07A7;
  glyphCarEstate = $F07A8; glyphCarHatchback = $F07A9; glyphCarPickup = $F07AA;
  glyphCarSide = $F07AB; glyphCarSports = $F07AC; glyphCaravan = $F07AD;
  glyphCctv = $F07AE; glyphChartDonut = $F07AF; glyphChartDonutVariant = $F07B0;
  glyphChartLineVariant = $F07B1; glyphChiliHot = $F07B2; glyphChiliMedium = $F07B3;
  glyphChiliMild = $F07B4; glyphCloudBraces = $F07B5; glyphCloudTags = $F07B6;
  glyphConsoleLine = $F07B7; glyphCorn = $F07B8; glyphFolderZipOutline = $F07B9;
  glyphCurrencyCny = $F07BA; glyphCurrencyEth = $F07BB; glyphCurrencyKrw = $F07BD;
  glyphCurrencySign = $F07BE; glyphCurrencyTwd = $F07BF; glyphDesktopClassic = $F07C0;
  glyphDipSwitch = $F07C1; glyphDonkey = $F07C2; glyphDotsHorizontalCircle = $F07C3;
  glyphDotsVerticalCircle = $F07C4; glyphEarHearing = $F07C5; glyphElephant = $F07C6;
  glyphStorefront = $F07C7; glyphFoodCroissant = $F07C8; glyphForklift = $F07C9;
  glyphFuel = $F07CA; glyphGesture = $F07CB; glyphGoogleAnalytics = $F07CC;
  glyphGoogleAssistant = $F07CD; glyphHeadphonesOff = $F07CE; glyphHighDefinition = $F07CF;
  glyphHomeAssistant = $F07D0; glyphHomeAutomation = $F07D1; glyphHomeCircle = $F07D2;
  glyphLanguageGo = $F07D3; glyphLanguageR = $F07D4; glyphLavaLamp = $F07D5;
  glyphLedStrip = $F07D6; glyphLocker = $F07D7; glyphLockerMultiple = $F07D8;
  glyphMapMarkerOutline = $F07D9; glyphMetronome = $F07DA; glyphMetronomeTick = $F07DB;
  glyphMicroSd = $F07DC; glyphFacebookGaming = $F07DD; glyphMovieRoll = $F07DE;
  glyphMushroom = $F07DF; glyphMushroomOutline = $F07E0; glyphNintendoSwitch = $F07E1;
  glyphNull = $F07E2; glyphPassport = $F07E3; glyphMoleculeCo2 = $F07E4;
  glyphPipe = $F07E5; glyphPipeDisconnected = $F07E6; glyphPowerSocketEu = $F07E7;
  glyphPowerSocketUk = $F07E8; glyphPowerSocketUs = $F07E9; glyphRice = $F07EA;
  glyphRing = $F07EB; glyphSass = $F07EC; glyphSendLock = $F07ED;
  glyphSoySauce = $F07EE; glyphStandardDefinition = $F07EF; glyphSurroundSound20 = $F07F0;
  glyphSurroundSound31 = $F07F1; glyphSurroundSound51 = $F07F2; glyphSurroundSound71 = $F07F3;
  glyphTelevisionClassic = $F07F4; glyphFormTextboxPassword = $F07F5; glyphThoughtBubble = $F07F6;
  glyphThoughtBubbleOutline = $F07F7; glyphTrackpad = $F07F8; glyphUltraHighDefinition = $F07F9;
  glyphVanPassenger = $F07FA; glyphVanUtility = $F07FB; glyphVanish = $F07FC;
  glyphVideo3d = $F07FD; glyphWall = $F07FE; glyphXmpp = $F07FF;
  glyphAccountMultiplePlusOutline = $F0800; glyphAccountPlusOutline = $F0801; glyphCreditCardWireless = $F0802;
  glyphAccountMusic = $F0803; glyphAtlassian = $F0804; glyphMicrosoftAzure = $F0805;
  glyphBasketball = $F0806; glyphBatteryChargingWireless = $F0807; glyphBatteryChargingWireless10 = $F0808;
  glyphBatteryChargingWireless20 = $F0809; glyphBatteryChargingWireless30 = $F080A; glyphBatteryChargingWireless40 = $F080B;
  glyphBatteryChargingWireless50 = $F080C; glyphBatteryChargingWireless60 = $F080D; glyphBatteryChargingWireless70 = $F080E;
  glyphBatteryChargingWireless80 = $F080F; glyphBatteryChargingWireless90 = $F0810; glyphBatteryChargingWirelessAlert = $F0811;
  glyphBatteryChargingWirelessOutline = $F0812; glyphBitcoin = $F0813; glyphBriefcaseOutline = $F0814;
  glyphCellphoneWireless = $F0815; glyphClover = $F0816; glyphCommentQuestion = $F0817;
  glyphContentSaveOutline = $F0818; glyphDeleteRestore = $F0819; glyphDoor = $F081A;
  glyphDoorClosed = $F081B; glyphDoorOpen = $F081C; glyphFanOff = $F081D;
  glyphFilePercent = $F081E; glyphFinance = $F081F; glyphLightningBoltCircle = $F0820;
  glyphFloorPlan = $F0821; glyphForumOutline = $F0822; glyphGolf = $F0823;
  glyphGoogleHome = $F0824; glyphGuyFawkesMask = $F0825; glyphHomeAccount = $F0826;
  glyphHomeHeart = $F0827; glyphHotTub = $F0828; glyphHulu = $F0829;
  glyphIceCream = $F082A; glyphImageOff = $F082B; glyphKarate = $F082C;
  glyphLadybug = $F082D; glyphNotebook = $F082E; glyphPhoneReturn = $F082F;
  glyphPokerChip = $F0830; glyphShape = $F0831; glyphShapeOutline = $F0832;
  glyphShipWheel = $F0833; glyphSoccerField = $F0834; glyphTableColumn = $F0835;
  glyphTableOfContents = $F0836; glyphTableRow = $F0837; glyphTableSettings = $F0838;
  glyphTelevisionBox = $F0839; glyphTelevisionClassicOff = $F083A; glyphTelevisionOff = $F083B;
  glyphTowTruck = $F083C; glyphUploadMultiple = $F083D; glyphVideo4kBox = $F083E;
  glyphVideoInputAntenna = $F083F; glyphVideoInputComponent = $F0840; glyphVideoInputHdmi = $F0841;
  glyphVideoInputSvideo = $F0842; glyphViewDashboardVariant = $F0843; glyphVuejs = $F0844;
  glyphXamarin = $F0845; glyphHumanMaleBoardPoll = $F0846; glyphYoutubeStudio = $F0847;
  glyphYoutubeGaming = $F0848; glyphAccountGroup = $F0849; glyphCameraSwitchOutline = $F084A;
  glyphAirport = $F084B; glyphArrowCollapseHorizontal = $F084C; glyphArrowCollapseVertical = $F084D;
  glyphArrowExpandHorizontal = $F084E; glyphArrowExpandVertical = $F084F; glyphAugmentedReality = $F0850;
  glyphBadminton = $F0851; glyphBaseball = $F0852; glyphBaseballBat = $F0853;
  glyphBottleWine = $F0854; glyphCheckOutline = $F0855; glyphCheckboxIntermediate = $F0856;
  glyphChessKing = $F0857; glyphChessKnight = $F0858; glyphChessPawn = $F0859;
  glyphChessQueen = $F085A; glyphChessRook = $F085B; glyphChessBishop = $F085C;
  glyphClipboardPulse = $F085D; glyphClipboardPulseOutline = $F085E; glyphCommentMultiple = $F085F;
  glyphCommentTextMultiple = $F0860; glyphCommentTextMultipleOutline = $F0861; glyphCrane = $F0862;
  glyphCurling = $F0863; glyphCurrencyBdt = $F0864; glyphCurrencyKzt = $F0865;
  glyphDatabaseSearch = $F0866; glyphDiceD12Outline = $F0867; glyphDocker = $F0868;
  glyphDoorbellVideo = $F0869; glyphEthereum = $F086A; glyphEyePlus = $F086B;
  glyphEyePlusOutline = $F086C; glyphEyeSettings = $F086D; glyphEyeSettingsOutline = $F086E;
  glyphFileQuestion = $F086F; glyphFolderNetwork = $F0870; glyphFunctionVariant = $F0871;
  glyphGarageAlert = $F0872; glyphGaugeEmpty = $F0873; glyphGaugeFull = $F0874;
  glyphGaugeLow = $F0875; glyphGlassWine = $F0876; glyphGraphql = $F0877;
  glyphHighDefinitionBox = $F0878; glyphHockeyPuck = $F0879; glyphHockeySticks = $F087A;
  glyphHomeAlert = $F087B; glyphImagePlus = $F087C; glyphJquery = $F087D;
  glyphLifebuoy = $F087E; glyphMixedReality = $F087F; glyphNativescript = $F0880;
  glyphOnepassword = $F0881; glyphPatreon = $F0882; glyphCloseCircleMultipleOutline = $F0883;
  glyphPeace = $F0884; glyphPhoneRotateLandscape = $F0885; glyphPhoneRotatePortrait = $F0886;
  glyphPier = $F0887; glyphPierCrane = $F0888; glyphPipeLeak = $F0889;
  glyphPiston = $F088A; glyphPlayNetwork = $F088B; glyphReminder = $F088C;
  glyphRoomService = $F088D; glyphSalesforce = $F088E; glyphShieldAccount = $F088F;
  glyphHumanMaleBoard = $F0890; glyphThermostatBox = $F0891; glyphTractor = $F0892;
  glyphVectorEllipse = $F0893; glyphVirtualReality = $F0894; glyphWatchExportVariant = $F0895;
  glyphWatchImportVariant = $F0896; glyphWatchVariant = $F0897; glyphWeatherHurricane = $F0898;
  glyphAccountHeart = $F0899; glyphAlien = $F089A; glyphAnvil = $F089B;
  glyphBatteryCharging10 = $F089C; glyphBatteryCharging50 = $F089D; glyphBatteryCharging70 = $F089E;
  glyphBatteryChargingOutline = $F089F; glyphBedEmpty = $F08A0; glyphBorderAllVariant = $F08A1;
  glyphBorderBottomVariant = $F08A2; glyphBorderLeftVariant = $F08A3; glyphBorderNoneVariant = $F08A4;
  glyphBorderRightVariant = $F08A5; glyphBorderTopVariant = $F08A6; glyphCalendarEdit = $F08A7;
  glyphClipboardCheckOutline = $F08A8; glyphConsoleNetwork = $F08A9; glyphFileCompare = $F08AA;
  glyphFireTruck = $F08AB; glyphFolderKey = $F08AC; glyphFolderKeyNetwork = $F08AD;
  glyphExpansionCard = $F08AE; glyphKayaking = $F08AF; glyphInboxMultiple = $F08B0;
  glyphLanguageLua = $F08B1; glyphLockSmart = $F08B2; glyphMicrophoneMinus = $F08B3;
  glyphMicrophonePlus = $F08B4; glyphPaletteSwatch = $F08B5; glyphPeriodicTable = $F08B6;
  glyphPickaxe = $F08B7; glyphQrcodeEdit = $F08B8; glyphRemoteDesktop = $F08B9;
  glyphSausage = $F08BA; glyphCogOutline = $F08BB; glyphSignalCellular1 = $F08BC;
  glyphSignalCellular2 = $F08BD; glyphSignalCellular3 = $F08BE; glyphSignalCellularOutline = $F08BF;
  glyphSsh = $F08C0; glyphSwapHorizontalVariant = $F08C1; glyphSwapVerticalVariant = $F08C2;
  glyphTooth = $F08C3; glyphTrainVariant = $F08C4; glyphAccountMultipleCheck = $F08C5;
  glyphApplication = $F08C6; glyphArch = $F08C7; glyphAxe = $F08C8;
  glyphBullseyeArrow = $F08C9; glyphBusClock = $F08CA; glyphCameraAccount = $F08CB;
  glyphCameraImage = $F08CC; glyphCarLimousine = $F08CD; glyphCardsClub = $F08CE;
  glyphCardsDiamond = $F08CF; glyphCardsSpade = $F08D1; glyphCellphoneText = $F08D2;
  glyphCellphoneMessage = $F08D3; glyphChartMultiline = $F08D4; glyphCircleEditOutline = $F08D5;
  glyphCogs = $F08D6; glyphCreditCardSettingsOutline = $F08D7; glyphDeathStar = $F08D8;
  glyphDeathStarVariant = $F08D9; glyphDebian = $F08DA; glyphFedora = $F08DB;
  glyphFileUndo = $F08DC; glyphFloorLamp = $F08DD; glyphFolderEdit = $F08DE;
  glyphFormatColumns = $F08DF; glyphFreebsd = $F08E0; glyphGateAnd = $F08E1;
  glyphGateNand = $F08E2; glyphGateNor = $F08E3; glyphGateNot = $F08E4;
  glyphGateOr = $F08E5; glyphGateXnor = $F08E6; glyphGateXor = $F08E7;
  glyphGentoo = $F08E8; glyphGlobeModel = $F08E9; glyphHammer = $F08EA;
  glyphHomeLock = $F08EB; glyphHomeLockOpen = $F08EC; glyphLinuxMint = $F08ED;
  glyphLockAlert = $F08EE; glyphLockQuestion = $F08EF; glyphMapMarkerDistance = $F08F0;
  glyphMidi = $F08F1; glyphMidiPort = $F08F2; glyphNas = $F08F3;
  glyphNetworkStrength1 = $F08F4; glyphNetworkStrength1Alert = $F08F5; glyphNetworkStrength2 = $F08F6;
  glyphNetworkStrength2Alert = $F08F7; glyphNetworkStrength3 = $F08F8; glyphNetworkStrength3Alert = $F08F9;
  glyphNetworkStrength4 = $F08FA; glyphNetworkStrength4Alert = $F08FB; glyphNetworkStrengthOff = $F08FC;
  glyphNetworkStrengthOffOutline = $F08FD; glyphNetworkStrengthOutline = $F08FE; glyphPlaySpeed = $F08FF;
  glyphPlaylistEdit = $F0900; glyphPowerCycle = $F0901; glyphPowerOff = $F0902;
  glyphPowerOn = $F0903; glyphPowerSleep = $F0904; glyphPowerSocketAu = $F0905;
  glyphPowerStandby = $F0906; glyphRabbit = $F0907; glyphRobotVacuumVariant = $F0908;
  glyphSatelliteUplink = $F0909; glyphScannerOff = $F090A; glyphBookMinusMultipleOutline = $F090B;
  glyphSquareEditOutline = $F090C; glyphSortNumericAscendingVariant = $F090D; glyphSteeringOff = $F090E;
  glyphTableSearch = $F090F; glyphTagMinus = $F0910; glyphTestTubeEmpty = $F0911;
  glyphTestTubeOff = $F0912; glyphTicketOutline = $F0913; glyphTrackLight = $F0914;
  glyphTransition = $F0915; glyphTransitionMasked = $F0916; glyphTumbleDryer = $F0917;
  glyphFileRefresh = $F0918; glyphVideoAccount = $F0919; glyphVideoImage = $F091A;
  glyphVideoStabilization = $F091B; glyphWallSconce = $F091C; glyphWallSconceFlat = $F091D;
  glyphWallSconceRoundVariant = $F091E; glyphWifiStrength1 = $F091F; glyphWifiStrength1Alert = $F0920;
  glyphWifiStrength1Lock = $F0921; glyphWifiStrength2 = $F0922; glyphWifiStrength2Alert = $F0923;
  glyphWifiStrength2Lock = $F0924; glyphWifiStrength3 = $F0925; glyphWifiStrength3Alert = $F0926;
  glyphWifiStrength3Lock = $F0927; glyphWifiStrength4 = $F0928; glyphWifiStrength4Alert = $F0929;
  glyphWifiStrength4Lock = $F092A; glyphWifiStrengthAlertOutline = $F092B; glyphWifiStrengthLockOutline = $F092C;
  glyphWifiStrengthOff = $F092D; glyphWifiStrengthOffOutline = $F092E; glyphWifiStrengthOutline = $F092F;
  glyphPinOffOutline = $F0930; glyphPinOutline = $F0931; glyphShareOutline = $F0932;
  glyphTrackpadLock = $F0933; glyphAccountBoxMultiple = $F0934; glyphAccountSearchOutline = $F0935;
  glyphAccountFilter = $F0936; glyphAngleAcute = $F0937; glyphAngleObtuse = $F0938;
  glyphAngleRight = $F0939; glyphAnimationPlay = $F093A; glyphArrowSplitHorizontal = $F093B;
  glyphArrowSplitVertical = $F093C; glyphAudioVideo = $F093D; glyphBattery10Bluetooth = $F093E;
  glyphBattery20Bluetooth = $F093F; glyphBattery30Bluetooth = $F0940; glyphBattery40Bluetooth = $F0941;
  glyphBattery50Bluetooth = $F0942; glyphBattery60Bluetooth = $F0943; glyphBattery70Bluetooth = $F0944;
  glyphBattery80Bluetooth = $F0945; glyphBattery90Bluetooth = $F0946; glyphBatteryAlertBluetooth = $F0947;
  glyphBatteryBluetooth = $F0948; glyphBatteryBluetoothVariant = $F0949; glyphBatteryUnknownBluetooth = $F094A;
  glyphDharmachakra = $F094B; glyphCalendarSearch = $F094C; glyphCellphoneRemove = $F094D;
  glyphCellphoneKey = $F094E; glyphCellphoneLock = $F094F; glyphCellphoneOff = $F0950;
  glyphCellphoneCog = $F0951; glyphCellphoneSound = $F0952; glyphCross = $F0953;
  glyphClock = $F0954; glyphClockAlert = $F0955; glyphCloudSearch = $F0956;
  glyphCloudSearchOutline = $F0957; glyphCordova = $F0958; glyphCryengine = $F0959;
  glyphCupcake = $F095A; glyphSineWave = $F095B; glyphCurrentDc = $F095C;
  glyphDatabaseImport = $F095D; glyphDatabaseExport = $F095E; glyphDeskLamp = $F095F;
  glyphDiscPlayer = $F0960; glyphEmailSearch = $F0961; glyphEmailSearchOutline = $F0962;
  glyphExponent = $F0963; glyphExponentBox = $F0964; glyphFileDownload = $F0965;
  glyphFileDownloadOutline = $F0966; glyphFirebase = $F0967; glyphFolderSearch = $F0968;
  glyphFolderSearchOutline = $F0969; glyphFormatListCheckbox = $F096A; glyphFountain = $F096B;
  glyphGoogleFit = $F096C; glyphGreaterThan = $F096D; glyphGreaterThanOrEqual = $F096E;
  glyphHardHat = $F096F; glyphHeadphonesBluetooth = $F0970; glyphHeartCircle = $F0971;
  glyphHeartCircleOutline = $F0972; glyphOm = $F0973; glyphHomeMinus = $F0974;
  glyphHomePlus = $F0975; glyphImageOutline = $F0976; glyphImageSearch = $F0977;
  glyphImageSearchOutline = $F0978; glyphStarCrescent = $F0979; glyphStarDavid = $F097A;
  glyphKeyboardOutline = $F097B; glyphLessThan = $F097C; glyphLessThanOrEqual = $F097D;
  glyphLightSwitch = $F097E; glyphLockClock = $F097F; glyphMagnifyClose = $F0980;
  glyphMapMinus = $F0981; glyphMapOutline = $F0982; glyphMapPlus = $F0983;
  glyphMapSearch = $F0984; glyphMapSearchOutline = $F0985; glyphMaterialDesign = $F0986;
  glyphMedal = $F0987; glyphMicrosoftDynamics365 = $F0988; glyphMonitorCellphone = $F0989;
  glyphMonitorCellphoneStar = $F098A; glyphMouseBluetooth = $F098B; glyphMuffin = $F098C;
  glyphNotEqual = $F098D; glyphNotEqualVariant = $F098E; glyphOrderBoolAscendingVariant = $F098F;
  glyphOrderBoolDescendingVariant = $F0990; glyphOfficeBuilding = $F0991; glyphPlusMinus = $F0992;
  glyphPlusMinusBox = $F0993; glyphPodcast = $F0994; glyphProgressCheck = $F0995;
  glyphProgressClock = $F0996; glyphProgressDownload = $F0997; glyphProgressUpload = $F0998;
  glyphQi = $F0999; glyphRecordPlayer = $F099A; glyphRestore = $F099B;
  glyphShieldOffOutline = $F099C; glyphShieldLock = $F099D; glyphShieldOff = $F099E;
  glyphSetTopBox = $F099F; glyphShower = $F09A0; glyphShowerHead = $F09A1;
  glyphSpeakerBluetooth = $F09A2; glyphSquareRootBox = $F09A3; glyphStarCircleOutline = $F09A4;
  glyphStarFace = $F09A5; glyphTableMergeCells = $F09A6; glyphTabletCellphone = $F09A7;
  glyphText = $F09A8; glyphTextShort = $F09A9; glyphTextLong = $F09AA;
  glyphToilet = $F09AB; glyphToolbox = $F09AC; glyphToolboxOutline = $F09AD;
  glyphTournament = $F09AE; glyphTwoFactorAuthentication = $F09AF; glyphUmbrellaClosed = $F09B0;
  glyphUnreal = $F09B1; glyphVideoMinus = $F09B2; glyphVideoPlus = $F09B3;
  glyphVolleyball = $F09B4; glyphWeightPound = $F09B5; glyphWhistle = $F09B6;
  glyphArrowBottomLeftBoldOutline = $F09B7; glyphArrowBottomLeftThick = $F09B8; glyphArrowBottomRightBoldOutline = $F09B9;
  glyphArrowBottomRightThick = $F09BA; glyphArrowDecision = $F09BB; glyphArrowDecisionAuto = $F09BC;
  glyphArrowDecisionAutoOutline = $F09BD; glyphArrowDecisionOutline = $F09BE; glyphArrowDownBoldOutline = $F09BF;
  glyphArrowLeftBoldOutline = $F09C0; glyphArrowLeftRightBoldOutline = $F09C1; glyphArrowRightBoldOutline = $F09C2;
  glyphArrowTopLeftBoldOutline = $F09C3; glyphArrowTopLeftThick = $F09C4; glyphArrowTopRightBoldOutline = $F09C5;
  glyphArrowTopRightThick = $F09C6; glyphArrowUpBoldOutline = $F09C7; glyphArrowUpDownBoldOutline = $F09C8;
  glyphBallot = $F09C9; glyphBallotOutline = $F09CA; glyphBetamax = $F09CB;
  glyphBookmarkMinus = $F09CC; glyphBookmarkMinusOutline = $F09CD; glyphBookmarkOff = $F09CE;
  glyphBookmarkOffOutline = $F09CF; glyphBraille = $F09D0; glyphBrain = $F09D1;
  glyphCalendarHeart = $F09D2; glyphCalendarStar = $F09D3; glyphCassette = $F09D4;
  glyphCellphoneArrowDown = $F09D5; glyphChevronDownBox = $F09D6; glyphChevronDownBoxOutline = $F09D7;
  glyphChevronLeftBox = $F09D8; glyphChevronLeftBoxOutline = $F09D9; glyphChevronRightBox = $F09DA;
  glyphChevronRightBoxOutline = $F09DB; glyphChevronUpBox = $F09DC; glyphChevronUpBoxOutline = $F09DD;
  glyphCircleMedium = $F09DE; glyphCircleSmall = $F09DF; glyphCloudAlert = $F09E0;
  glyphCommentArrowLeft = $F09E1; glyphCommentArrowLeftOutline = $F09E2; glyphCommentArrowRight = $F09E3;
  glyphCommentArrowRightOutline = $F09E4; glyphCommentPlus = $F09E5; glyphCurrencyPhp = $F09E6;
  glyphDeleteOutline = $F09E7; glyphDesktopMacDashboard = $F09E8; glyphDownloadMultiple = $F09E9;
  glyphEightTrack = $F09EA; glyphEmailPlus = $F09EB; glyphEmailPlusOutline = $F09EC;
  glyphTextBoxOutline = $F09ED; glyphFileDocumentOutline = $F09EE; glyphFloppyVariant = $F09EF;
  glyphFlowerOutline = $F09F0; glyphFlowerTulip = $F09F1; glyphFlowerTulipOutline = $F09F2;
  glyphFormatFontSizeDecrease = $F09F3; glyphFormatFontSizeIncrease = $F09F4; glyphGhostOff = $F09F5;
  glyphGoogleLens = $F09F6; glyphGoogleSpreadsheet = $F09F7; glyphImageMove = $F09F8;
  glyphKeyboardSettings = $F09F9; glyphKeyboardSettingsOutline = $F09FA; glyphKnife = $F09FB;
  glyphKnifeMilitary = $F09FC; glyphLayersOffOutline = $F09FD; glyphLayersOutline = $F09FE;
  glyphLighthouse = $F09FF; glyphLighthouseOn = $F0A00; glyphMapLegend = $F0A01;
  glyphMenuLeftOutline = $F0A02; glyphMenuRightOutline = $F0A03; glyphMessageAlertOutline = $F0A04;
  glyphMiniSd = $F0A05; glyphMinidisc = $F0A06; glyphMonitorDashboard = $F0A07;
  glyphPirate = $F0A08; glyphPokemonGo = $F0A09; glyphPowershell = $F0A0A;
  glyphPrinterWireless = $F0A0B; glyphQualityLow = $F0A0C; glyphQualityMedium = $F0A0D;
  glyphReflectHorizontal = $F0A0E; glyphReflectVertical = $F0A0F; glyphRhombusMedium = $F0A10;
  glyphRhombusSplit = $F0A11; glyphShieldAccountOutline = $F0A12; glyphSquareMedium = $F0A13;
  glyphSquareMediumOutline = $F0A14; glyphSquareSmall = $F0A15; glyphSubtitles = $F0A16;
  glyphSubtitlesOutline = $F0A17; glyphTableBorder = $F0A18; glyphToggleSwitchOffOutline = $F0A19;
  glyphToggleSwitchOutline = $F0A1A; glyphVhs = $F0A1B; glyphVideoVintage = $F0A1C;
  glyphViewDashboardOutline = $F0A1D; glyphMicrosoftVisualStudioCode = $F0A1E; glyphVote = $F0A1F;
  glyphVoteOutline = $F0A20; glyphMicrosoftWindowsClassic = $F0A21; glyphMicrosoftXboxControllerBatteryCharging = $F0A22;
  glyphZipDisk = $F0A23; glyphAspectRatio = $F0A24; glyphBabel = $F0A25;
  glyphBalloon = $F0A26; glyphBankTransfer = $F0A27; glyphBankTransferIn = $F0A28;
  glyphBankTransferOut = $F0A29; glyphBriefcaseMinus = $F0A2A; glyphBriefcasePlus = $F0A2B;
  glyphBriefcaseRemove = $F0A2C; glyphBriefcaseSearch = $F0A2D; glyphBugCheck = $F0A2E;
  glyphBugCheckOutline = $F0A2F; glyphBugOutline = $F0A30; glyphCalendarAlert = $F0A31;
  glyphCalendarMultiselect = $F0A32; glyphCalendarWeek = $F0A33; glyphCalendarWeekBegin = $F0A34;
  glyphCellphoneScreenshot = $F0A35; glyphCityVariant = $F0A36; glyphCityVariantOutline = $F0A37;
  glyphClipboardTextOutline = $F0A38; glyphCloudQuestion = $F0A39; glyphCommentEye = $F0A3A;
  glyphCommentEyeOutline = $F0A3B; glyphCommentSearch = $F0A3C; glyphCommentSearchOutline = $F0A3D;
  glyphContain = $F0A3E; glyphContainEnd = $F0A3F; glyphContainStart = $F0A40;
  glyphDlna = $F0A41; glyphDoctor = $F0A42; glyphDog = $F0A43;
  glyphDogSide = $F0A44; glyphEarHearingOff = $F0A45; glyphEngineOff = $F0A46;
  glyphEngineOffOutline = $F0A47; glyphExitRun = $F0A48; glyphFeatureSearch = $F0A49;
  glyphFeatureSearchOutline = $F0A4A; glyphFileAlert = $F0A4B; glyphFileAlertOutline = $F0A4C;
  glyphFileUpload = $F0A4D; glyphFileUploadOutline = $F0A4E; glyphHandFrontRight = $F0A4F;
  glyphHandOkay = $F0A50; glyphHandPeace = $F0A51; glyphHandPeaceVariant = $F0A52;
  glyphHandPointingDown = $F0A53; glyphHandPointingLeft = $F0A54; glyphHandPointingUp = $F0A55;
  glyphHeartMultiple = $F0A56; glyphHeartMultipleOutline = $F0A57; glyphHorseshoe = $F0A58;
  glyphHumanFemaleBoy = $F0A59; glyphHumanFemaleFemale = $F0A5A; glyphHumanFemaleGirl = $F0A5B;
  glyphHumanMaleBoy = $F0A5C; glyphHumanMaleGirl = $F0A5D; glyphHumanMaleMale = $F0A5E;
  glyphIp = $F0A5F; glyphIpNetwork = $F0A60; glyphLitecoin = $F0A61;
  glyphMagnifyMinusCursor = $F0A62; glyphMagnifyPlusCursor = $F0A63; glyphMenuSwap = $F0A64;
  glyphMenuSwapOutline = $F0A65; glyphPuzzleOutline = $F0A66; glyphRegisteredTrademark = $F0A67;
  glyphResize = $F0A68; glyphRouterWirelessSettings = $F0A69; glyphSafe = $F0A6A;
  glyphScissorsCutting = $F0A6B; glyphSelectDrag = $F0A6C; glyphSelectionDrag = $F0A6D;
  glyphSettingsHelper = $F0A6E; glyphSignal5g = $F0A6F; glyphSilverwareForkKnife = $F0A70;
  glyphSmog = $F0A71; glyphSolarPower = $F0A72; glyphStarBox = $F0A73;
  glyphStarBoxOutline = $F0A74; glyphTablePlus = $F0A75; glyphTableRemove = $F0A76;
  glyphTargetVariant = $F0A77; glyphTrademark = $F0A78; glyphTrashCan = $F0A79;
  glyphTrashCanOutline = $F0A7A; glyphTshirtCrew = $F0A7B; glyphTshirtV = $F0A7C;
  glyphZodiacAquarius = $F0A7D; glyphZodiacAries = $F0A7E; glyphZodiacCancer = $F0A7F;
  glyphZodiacCapricorn = $F0A80; glyphZodiacGemini = $F0A81; glyphZodiacLeo = $F0A82;
  glyphZodiacLibra = $F0A83; glyphZodiacPisces = $F0A84; glyphZodiacSagittarius = $F0A85;
  glyphZodiacScorpio = $F0A86; glyphZodiacTaurus = $F0A87; glyphZodiacVirgo = $F0A88;
  glyphAccountChild = $F0A89; glyphAccountChildCircle = $F0A8A; glyphAccountSupervisor = $F0A8B;
  glyphAccountSupervisorCircle = $F0A8C; glyphAmpersand = $F0A8D; glyphWebOff = $F0A8E;
  glyphAnimationOutline = $F0A8F; glyphAnimationPlayOutline = $F0A90; glyphBellOffOutline = $F0A91;
  glyphBellPlusOutline = $F0A92; glyphBellSleepOutline = $F0A93; glyphBookMinusMultiple = $F0A94;
  glyphBookPlusMultiple = $F0A95; glyphBookRemoveMultiple = $F0A96; glyphBookRemove = $F0A97;
  glyphBriefcaseEdit = $F0A98; glyphBusAlert = $F0A99; glyphCalculatorVariant = $F0A9A;
  glyphCapsLock = $F0A9B; glyphCashRefund = $F0A9C; glyphCheckbook = $F0A9D;
  glyphCircleSlice1 = $F0A9E; glyphCircleSlice2 = $F0A9F; glyphCircleSlice3 = $F0AA0;
  glyphCircleSlice4 = $F0AA1; glyphCircleSlice5 = $F0AA2; glyphCircleSlice6 = $F0AA3;
  glyphCircleSlice7 = $F0AA4; glyphCircleSlice8 = $F0AA5; glyphCollapseAll = $F0AA6;
  glyphCollapseAllOutline = $F0AA7; glyphCreditCardRefundOutline = $F0AA8; glyphDatabaseCheck = $F0AA9;
  glyphDatabaseLock = $F0AAA; glyphDesktopTowerMonitor = $F0AAB; glyphDishwasher = $F0AAC;
  glyphDogService = $F0AAD; glyphDotNet = $F0AAE; glyphEgg = $F0AAF;
  glyphEggEaster = $F0AB0; glyphEmailCheck = $F0AB1; glyphEmailCheckOutline = $F0AB2;
  glyphEt = $F0AB3; glyphExpandAll = $F0AB4; glyphExpandAllOutline = $F0AB5;
  glyphFileCabinet = $F0AB6; glyphTextBoxMultiple = $F0AB7; glyphTextBoxMultipleOutline = $F0AB8;
  glyphFileMove = $F0AB9; glyphFolderClock = $F0ABA; glyphFolderClockOutline = $F0ABB;
  glyphFormatAnnotationMinus = $F0ABC; glyphGesturePinch = $F0ABD; glyphGestureSpread = $F0ABE;
  glyphGestureSwipeHorizontal = $F0ABF; glyphGestureSwipeVertical = $F0AC0; glyphHail = $F0AC1;
  glyphHelicopter = $F0AC2; glyphHexagonSlice1 = $F0AC3; glyphHexagonSlice2 = $F0AC4;
  glyphHexagonSlice3 = $F0AC5; glyphHexagonSlice4 = $F0AC6; glyphHexagonSlice5 = $F0AC7;
  glyphHexagonSlice6 = $F0AC8; glyphHexagram = $F0AC9; glyphHexagramOutline = $F0ACA;
  glyphLabelOff = $F0ACB; glyphLabelOffOutline = $F0ACC; glyphLabelVariant = $F0ACD;
  glyphLabelVariantOutline = $F0ACE; glyphLanguageRubyOnRails = $F0ACF; glyphLaravel = $F0AD0;
  glyphMastodon = $F0AD1; glyphSortNumericDescendingVariant = $F0AD2; glyphMinusCircleMultipleOutline = $F0AD3;
  glyphMusicCircleOutline = $F0AD4; glyphPinwheel = $F0AD5; glyphPinwheelOutline = $F0AD6;
  glyphRadiatorDisabled = $F0AD7; glyphRadiatorOff = $F0AD8; glyphSelectCompare = $F0AD9;
  glyphShieldPlus = $F0ADA; glyphShieldPlusOutline = $F0ADB; glyphShieldRemove = $F0ADC;
  glyphShieldRemoveOutline = $F0ADD; glyphBookPlusMultipleOutline = $F0ADE; glyphSinaWeibo = $F0ADF;
  glyphSprayBottle = $F0AE0; glyphSqueegee = $F0AE1; glyphStarFourPoints = $F0AE2;
  glyphStarFourPointsOutline = $F0AE3; glyphStarThreePoints = $F0AE4; glyphStarThreePointsOutline = $F0AE5;
  glyphSymfony = $F0AE6; glyphVariable = $F0AE7; glyphVectorBezier = $F0AE8;
  glyphWiper = $F0AE9; glyphZWave = $F0AEA; glyphZend = $F0AEB;
  glyphAccountMinusOutline = $F0AEC; glyphAccountRemoveOutline = $F0AED; glyphAlphaA = $F0AEE;
  glyphAlphaB = $F0AEF; glyphAlphaC = $F0AF0; glyphAlphaD = $F0AF1;
  glyphAlphaE = $F0AF2; glyphAlphaF = $F0AF3; glyphAlphaG = $F0AF4;
  glyphAlphaH = $F0AF5; glyphAlphaI = $F0AF6; glyphAlphaJ = $F0AF7;
  glyphAlphaK = $F0AF8; glyphAlphaL = $F0AF9; glyphAlphaM = $F0AFA;
  glyphAlphaN = $F0AFB; glyphAlphaO = $F0AFC; glyphAlphaP = $F0AFD;
  glyphAlphaQ = $F0AFE; glyphAlphaR = $F0AFF; glyphAlphaS = $F0B00;
  glyphAlphaT = $F0B01; glyphAlphaU = $F0B02; glyphAlphaV = $F0B03;
  glyphAlphaW = $F0B04; glyphAlphaX = $F0B05; glyphAlphaY = $F0B06;
  glyphAlphaZ = $F0B07; glyphAlphaABox = $F0B08; glyphAlphaBBox = $F0B09;
  glyphAlphaCBox = $F0B0A; glyphAlphaDBox = $F0B0B; glyphAlphaEBox = $F0B0C;
  glyphAlphaFBox = $F0B0D; glyphAlphaGBox = $F0B0E; glyphAlphaHBox = $F0B0F;
  glyphAlphaIBox = $F0B10; glyphAlphaJBox = $F0B11; glyphAlphaKBox = $F0B12;
  glyphAlphaLBox = $F0B13; glyphAlphaMBox = $F0B14; glyphAlphaNBox = $F0B15;
  glyphAlphaOBox = $F0B16; glyphAlphaPBox = $F0B17; glyphAlphaQBox = $F0B18;
  glyphAlphaRBox = $F0B19; glyphAlphaSBox = $F0B1A; glyphAlphaTBox = $F0B1B;
  glyphAlphaUBox = $F0B1C; glyphAlphaVBox = $F0B1D; glyphAlphaWBox = $F0B1E;
  glyphAlphaXBox = $F0B1F; glyphAlphaYBox = $F0B20; glyphAlphaZBox = $F0B21;
  glyphBulldozer = $F0B22; glyphBullhornOutline = $F0B23; glyphCalendarExport = $F0B24;
  glyphCalendarImport = $F0B25; glyphChevronDownCircle = $F0B26; glyphChevronDownCircleOutline = $F0B27;
  glyphChevronLeftCircle = $F0B28; glyphChevronLeftCircleOutline = $F0B29; glyphChevronRightCircle = $F0B2A;
  glyphChevronRightCircleOutline = $F0B2B; glyphChevronUpCircle = $F0B2C; glyphChevronUpCircleOutline = $F0B2D;
  glyphContentSaveSettingsOutline = $F0B2E; glyphCrystalBall = $F0B2F; glyphEmber = $F0B30;
  glyphFacebookWorkplace = $F0B31; glyphFileReplace = $F0B32; glyphFileReplaceOutline = $F0B33;
  glyphFormatLetterCase = $F0B34; glyphFormatLetterCaseLower = $F0B35; glyphFormatLetterCaseUpper = $F0B36;
  glyphLanguageJava = $F0B37; glyphCircleMultiple = $F0B38; glyphNumeric1 = $F0B3A;
  glyphNumeric2 = $F0B3B; glyphNumeric3 = $F0B3C; glyphNumeric4 = $F0B3D;
  glyphNumeric5 = $F0B3E; glyphNumeric6 = $F0B3F; glyphNumeric7 = $F0B40;
  glyphNumeric8 = $F0B41; glyphNumeric9 = $F0B42; glyphOrigin = $F0B43;
  glyphResistor = $F0B44; glyphResistorNodes = $F0B45; glyphRobotIndustrial = $F0B46;
  glyphShoeFormal = $F0B47; glyphShoeHeel = $F0B48; glyphSilo = $F0B49;
  glyphBoxCutterOff = $F0B4A; glyphTabMinus = $F0B4B; glyphTabRemove = $F0B4C;
  glyphTapeMeasure = $F0B4D; glyphTelescope = $F0B4E; glyphYahoo = $F0B4F;
  glyphAccountAlertOutline = $F0B50; glyphAccountArrowLeft = $F0B51; glyphAccountArrowLeftOutline = $F0B52;
  glyphAccountArrowRight = $F0B53; glyphAccountArrowRightOutline = $F0B54; glyphAccountCircleOutline = $F0B55;
  glyphAccountClock = $F0B56; glyphAccountClockOutline = $F0B57; glyphAccountGroupOutline = $F0B58;
  glyphAccountQuestion = $F0B59; glyphAccountQuestionOutline = $F0B5A; glyphArtstation = $F0B5B;
  glyphBackspaceOutline = $F0B5C; glyphBarleyOff = $F0B5D; glyphBarn = $F0B5E;
  glyphBat = $F0B5F; glyphApplicationSettings = $F0B60; glyphBilliards = $F0B61;
  glyphBilliardsRack = $F0B62; glyphBookOpenOutline = $F0B63; glyphBookOutline = $F0B64;
  glyphBoxingGlove = $F0B65; glyphCalendarBlankOutline = $F0B66; glyphCalendarOutline = $F0B67;
  glyphCalendarRangeOutline = $F0B68; glyphCameraControl = $F0B69; glyphCameraEnhanceOutline = $F0B6A;
  glyphCarDoor = $F0B6B; glyphCarElectric = $F0B6C; glyphCarKey = $F0B6D;
  glyphCarMultiple = $F0B6E; glyphCard = $F0B6F; glyphCardBulleted = $F0B70;
  glyphCardBulletedOff = $F0B71; glyphCardBulletedOffOutline = $F0B72; glyphCardBulletedOutline = $F0B73;
  glyphCardBulletedSettings = $F0B74; glyphCardBulletedSettingsOutline = $F0B75; glyphCardOutline = $F0B76;
  glyphCardText = $F0B77; glyphCardTextOutline = $F0B78; glyphChat = $F0B79;
  glyphChatAlert = $F0B7A; glyphChatProcessing = $F0B7B; glyphChefHat = $F0B7C;
  glyphCloudDownloadOutline = $F0B7D; glyphCloudUploadOutline = $F0B7E; glyphCoffin = $F0B7F;
  glyphCompassOff = $F0B80; glyphCompassOffOutline = $F0B81; glyphControllerClassic = $F0B82;
  glyphControllerClassicOutline = $F0B83; glyphCubeScan = $F0B84; glyphCurrencyBrl = $F0B85;
  glyphDatabaseEdit = $F0B86; glyphDeathlyHallows = $F0B87; glyphDeleteCircleOutline = $F0B88;
  glyphDeleteForeverOutline = $F0B89; glyphDiamond = $F0B8A; glyphDiamondOutline = $F0B8B;
  glyphDnsOutline = $F0B8C; glyphDotsHorizontalCircleOutline = $F0B8D; glyphDotsVerticalCircleOutline = $F0B8E;
  glyphDownloadOutline = $F0B8F; glyphDragVariant = $F0B90; glyphEjectOutline = $F0B91;
  glyphEmailMarkAsUnread = $F0B92; glyphExportVariant = $F0B93; glyphEyeCircle = $F0B94;
  glyphEyeCircleOutline = $F0B95; glyphFaceManOutline = $F0B96; glyphFileFindOutline = $F0B97;
  glyphFileRemove = $F0B98; glyphFlagMinus = $F0B99; glyphFlagPlus = $F0B9A;
  glyphFlagRemove = $F0B9B; glyphFolderAccountOutline = $F0B9C; glyphFolderPlusOutline = $F0B9D;
  glyphFolderRemoveOutline = $F0B9E; glyphFolderStarOutline = $F0B9F; glyphGitlab = $F0BA0;
  glyphGog = $F0BA1; glyphGraveStone = $F0BA2; glyphHalloween = $F0BA3;
  glyphHatFedora = $F0BA4; glyphHelpRhombus = $F0BA5; glyphHelpRhombusOutline = $F0BA6;
  glyphHomeVariantOutline = $F0BA7; glyphInboxMultipleOutline = $F0BA8; glyphLibraryShelves = $F0BA9;
  glyphMapbox = $F0BAA; glyphMenuOpen = $F0BAB; glyphMolecule = $F0BAC;
  glyphOneUp = $F0BAD; glyphOpenSourceInitiative = $F0BAE; glyphPacMan = $F0BAF;
  glyphPageNext = $F0BB0; glyphPageNextOutline = $F0BB1; glyphPagePrevious = $F0BB2;
  glyphPagePreviousOutline = $F0BB3; glyphPan = $F0BB4; glyphPanBottomLeft = $F0BB5;
  glyphPanBottomRight = $F0BB6; glyphPanDown = $F0BB7; glyphPanHorizontal = $F0BB8;
  glyphPanLeft = $F0BB9; glyphPanRight = $F0BBA; glyphPanTopLeft = $F0BBB;
  glyphPanTopRight = $F0BBC; glyphPanUp = $F0BBD; glyphPanVertical = $F0BBE;
  glyphPumpkin = $F0BBF; glyphRollupjs = $F0BC0; glyphScript = $F0BC1;
  glyphScriptText = $F0BC2; glyphScriptTextOutline = $F0BC3; glyphShieldKey = $F0BC4;
  glyphShieldKeyOutline = $F0BC5; glyphSkullCrossbones = $F0BC6; glyphSkullCrossbonesOutline = $F0BC7;
  glyphSkullOutline = $F0BC8; glyphSpaceInvaders = $F0BC9; glyphSpiderWeb = $F0BCA;
  glyphViewSplitHorizontal = $F0BCB; glyphViewSplitVertical = $F0BCC; glyphSwapHorizontalBold = $F0BCD;
  glyphSwapVerticalBold = $F0BCE; glyphTagHeartOutline = $F0BCF; glyphTargetAccount = $F0BD0;
  glyphTimeline = $F0BD1; glyphTimelineOutline = $F0BD2; glyphTimelineText = $F0BD3;
  glyphTimelineTextOutline = $F0BD4; glyphTooltipImageOutline = $F0BD5; glyphTooltipPlus = $F0BD6;
  glyphTooltipTextOutline = $F0BD7; glyphTrainCar = $F0BD8; glyphTriforce = $F0BD9;
  glyphUbisoft = $F0BDA; glyphVideoOffOutline = $F0BDB; glyphVideoOutline = $F0BDC;
  glyphWalletOutline = $F0BDD; glyphWaze = $F0BDE; glyphWrapDisabled = $F0BDF;
  glyphWrenchOutline = $F0BE0; glyphAccessPointNetworkOff = $F0BE1; glyphAccountCheckOutline = $F0BE2;
  glyphAccountHeartOutline = $F0BE3; glyphAccountKeyOutline = $F0BE4; glyphAccountMultipleMinusOutline = $F0BE5;
  glyphAccountNetworkOutline = $F0BE6; glyphAccountOffOutline = $F0BE7; glyphAccountStarOutline = $F0BE8;
  glyphAirbag = $F0BE9; glyphAlarmLightOutline = $F0BEA; glyphAlphaABoxOutline = $F0BEB;
  glyphAlphaACircle = $F0BEC; glyphAlphaACircleOutline = $F0BED; glyphAlphaBBoxOutline = $F0BEE;
  glyphAlphaBCircle = $F0BEF; glyphAlphaBCircleOutline = $F0BF0; glyphAlphaCBoxOutline = $F0BF1;
  glyphAlphaCCircle = $F0BF2; glyphAlphaCCircleOutline = $F0BF3; glyphAlphaDBoxOutline = $F0BF4;
  glyphAlphaDCircle = $F0BF5; glyphAlphaDCircleOutline = $F0BF6; glyphAlphaEBoxOutline = $F0BF7;
  glyphAlphaECircle = $F0BF8; glyphAlphaECircleOutline = $F0BF9; glyphAlphaFBoxOutline = $F0BFA;
  glyphAlphaFCircle = $F0BFB; glyphAlphaFCircleOutline = $F0BFC; glyphAlphaGBoxOutline = $F0BFD;
  glyphAlphaGCircle = $F0BFE; glyphAlphaGCircleOutline = $F0BFF; glyphAlphaHBoxOutline = $F0C00;
  glyphAlphaHCircle = $F0C01; glyphAlphaHCircleOutline = $F0C02; glyphAlphaIBoxOutline = $F0C03;
  glyphAlphaICircle = $F0C04; glyphAlphaICircleOutline = $F0C05; glyphAlphaJBoxOutline = $F0C06;
  glyphAlphaJCircle = $F0C07; glyphAlphaJCircleOutline = $F0C08; glyphAlphaKBoxOutline = $F0C09;
  glyphAlphaKCircle = $F0C0A; glyphAlphaKCircleOutline = $F0C0B; glyphAlphaLBoxOutline = $F0C0C;
  glyphAlphaLCircle = $F0C0D; glyphAlphaLCircleOutline = $F0C0E; glyphAlphaMBoxOutline = $F0C0F;
  glyphAlphaMCircle = $F0C10; glyphAlphaMCircleOutline = $F0C11; glyphAlphaNBoxOutline = $F0C12;
  glyphAlphaNCircle = $F0C13; glyphAlphaNCircleOutline = $F0C14; glyphAlphaOBoxOutline = $F0C15;
  glyphAlphaOCircle = $F0C16; glyphAlphaOCircleOutline = $F0C17; glyphAlphaPBoxOutline = $F0C18;
  glyphAlphaPCircle = $F0C19; glyphAlphaPCircleOutline = $F0C1A; glyphAlphaQBoxOutline = $F0C1B;
  glyphAlphaQCircle = $F0C1C; glyphAlphaQCircleOutline = $F0C1D; glyphAlphaRBoxOutline = $F0C1E;
  glyphAlphaRCircle = $F0C1F; glyphAlphaRCircleOutline = $F0C20; glyphAlphaSBoxOutline = $F0C21;
  glyphAlphaSCircle = $F0C22; glyphAlphaSCircleOutline = $F0C23; glyphAlphaTBoxOutline = $F0C24;
  glyphAlphaTCircle = $F0C25; glyphAlphaTCircleOutline = $F0C26; glyphAlphaUBoxOutline = $F0C27;
  glyphAlphaUCircle = $F0C28; glyphAlphaUCircleOutline = $F0C29; glyphAlphaVBoxOutline = $F0C2A;
  glyphAlphaVCircle = $F0C2B; glyphAlphaVCircleOutline = $F0C2C; glyphAlphaWBoxOutline = $F0C2D;
  glyphAlphaWCircle = $F0C2E; glyphAlphaWCircleOutline = $F0C2F; glyphAlphaXBoxOutline = $F0C30;
  glyphAlphaXCircle = $F0C31; glyphAlphaXCircleOutline = $F0C32; glyphAlphaYBoxOutline = $F0C33;
  glyphAlphaYCircle = $F0C34; glyphAlphaYCircleOutline = $F0C35; glyphAlphaZBoxOutline = $F0C36;
  glyphAlphaZCircle = $F0C37; glyphAlphaZCircleOutline = $F0C38; glyphBallotRecount = $F0C39;
  glyphBallotRecountOutline = $F0C3A; glyphBasketballHoop = $F0C3B; glyphBasketballHoopOutline = $F0C3C;
  glyphBriefcaseDownloadOutline = $F0C3D; glyphBriefcaseEditOutline = $F0C3E; glyphBriefcaseMinusOutline = $F0C3F;
  glyphBriefcasePlusOutline = $F0C40; glyphBriefcaseRemoveOutline = $F0C41; glyphBriefcaseSearchOutline = $F0C42;
  glyphBriefcaseUploadOutline = $F0C43; glyphCalendarCheckOutline = $F0C44; glyphCalendarRemoveOutline = $F0C45;
  glyphCalendarTextOutline = $F0C46; glyphCarBrakeAbs = $F0C47; glyphCarBrakeAlert = $F0C48;
  glyphCarEsp = $F0C49; glyphCarLightDimmed = $F0C4A; glyphCarLightFog = $F0C4B;
  glyphCarLightHigh = $F0C4C; glyphCarTireAlert = $F0C4D; glyphCartArrowRight = $F0C4E;
  glyphCharity = $F0C4F; glyphChartBellCurve = $F0C50; glyphCheckboxMultipleOutline = $F0C51;
  glyphCheckboxOutline = $F0C52; glyphCheckNetwork = $F0C53; glyphCheckNetworkOutline = $F0C54;
  glyphClipboardAccountOutline = $F0C55; glyphClipboardArrowDownOutline = $F0C56; glyphClipboardArrowUp = $F0C57;
  glyphClipboardArrowUpOutline = $F0C58; glyphClipboardPlay = $F0C59; glyphClipboardPlayOutline = $F0C5A;
  glyphClipboardTextPlay = $F0C5B; glyphClipboardTextPlayOutline = $F0C5C; glyphCloseBoxMultiple = $F0C5D;
  glyphCloseBoxMultipleOutline = $F0C5E; glyphCloseNetworkOutline = $F0C5F; glyphConsoleNetworkOutline = $F0C60;
  glyphCurrencyIls = $F0C61; glyphDeleteSweepOutline = $F0C62; glyphDiameter = $F0C63;
  glyphDiameterOutline = $F0C64; glyphDiameterVariant = $F0C65; glyphDownloadNetworkOutline = $F0C66;
  glyphDumpTruck = $F0C67; glyphEmoticon = $F0C68; glyphEmoticonAngry = $F0C69;
  glyphEmoticonAngryOutline = $F0C6A; glyphEmoticonCool = $F0C6B; glyphEmoticonCry = $F0C6C;
  glyphEmoticonCryOutline = $F0C6D; glyphEmoticonDead = $F0C6E; glyphEmoticonDevil = $F0C6F;
  glyphEmoticonExcited = $F0C70; glyphEmoticonHappy = $F0C71; glyphEmoticonKiss = $F0C72;
  glyphEmoticonKissOutline = $F0C73; glyphEmoticonNeutral = $F0C74; glyphEmoticonPoopOutline = $F0C75;
  glyphEmoticonSad = $F0C76; glyphEmoticonTongueOutline = $F0C77; glyphEmoticonWink = $F0C78;
  glyphEmoticonWinkOutline = $F0C79; glyphEslint = $F0C7A; glyphFaceRecognition = $F0C7B;
  glyphFileSearch = $F0C7C; glyphFileSearchOutline = $F0C7D; glyphFileTable = $F0C7E;
  glyphFileTableOutline = $F0C7F; glyphFolderKeyNetworkOutline = $F0C80; glyphFolderNetworkOutline = $F0C81;
  glyphFolderText = $F0C82; glyphFolderTextOutline = $F0C83; glyphFoodAppleOutline = $F0C84;
  glyphFuse = $F0C85; glyphFuseBlade = $F0C86; glyphGoogleAds = $F0C87;
  glyphGoogleStreetView = $F0C88; glyphHazardLights = $F0C89; glyphHelpNetworkOutline = $F0C8A;
  glyphApplicationBrackets = $F0C8B; glyphApplicationBracketsOutline = $F0C8C; glyphImageSizeSelectActual = $F0C8D;
  glyphImageSizeSelectLarge = $F0C8E; glyphImageSizeSelectSmall = $F0C8F; glyphIpNetworkOutline = $F0C90;
  glyphIpod = $F0C91; glyphLanguageHaskell = $F0C92; glyphLeafMaple = $F0C93;
  glyphLinkPlus = $F0C94; glyphMapMarkerCheck = $F0C95; glyphMathCos = $F0C96;
  glyphMathSin = $F0C97; glyphMathTan = $F0C98; glyphMicrowave = $F0C99;
  glyphMinusNetworkOutline = $F0C9A; glyphNetworkOff = $F0C9B; glyphNetworkOffOutline = $F0C9C;
  glyphNetworkOutline = $F0C9D; glyphNumeric1Circle = $F0CA0; glyphNumeric1CircleOutline = $F0CA1;
  glyphNumeric2Circle = $F0CA2; glyphNumeric2CircleOutline = $F0CA3; glyphNumeric3Circle = $F0CA4;
  glyphNumeric3CircleOutline = $F0CA5; glyphNumeric4Circle = $F0CA6; glyphNumeric4CircleOutline = $F0CA7;
  glyphNumeric5Circle = $F0CA8; glyphNumeric5CircleOutline = $F0CA9; glyphNumeric6Circle = $F0CAA;
  glyphNumeric6CircleOutline = $F0CAB; glyphNumeric7Circle = $F0CAC; glyphNumeric7CircleOutline = $F0CAD;
  glyphNumeric8Circle = $F0CAE; glyphNumeric8CircleOutline = $F0CAF; glyphNumeric9Circle = $F0CB0;
  glyphNumeric9CircleOutline = $F0CB1; glyphNumeric9PlusCircle = $F0CB2; glyphNumeric9PlusCircleOutline = $F0CB3;
  glyphParachute = $F0CB4; glyphParachuteOutline = $F0CB5; glyphPencilOutline = $F0CB6;
  glyphPlayNetworkOutline = $F0CB7; glyphPlaylistMusic = $F0CB8; glyphPlaylistMusicOutline = $F0CB9;
  glyphPlusNetworkOutline = $F0CBA; glyphPostageStamp = $F0CBB; glyphProgressAlert = $F0CBC;
  glyphProgressWrench = $F0CBD; glyphRadioAm = $F0CBE; glyphRadioFm = $F0CBF;
  glyphRadius = $F0CC0; glyphRadiusOutline = $F0CC1; glyphRulerSquare = $F0CC2;
  glyphSeat = $F0CC3; glyphSeatOutline = $F0CC4; glyphSeatbelt = $F0CC5;
  glyphSheep = $F0CC6; glyphShieldAirplaneOutline = $F0CC7; glyphShieldCheckOutline = $F0CC8;
  glyphShieldCross = $F0CC9; glyphShieldCrossOutline = $F0CCA; glyphShieldHomeOutline = $F0CCB;
  glyphShieldLockOutline = $F0CCC; glyphSortVariantLock = $F0CCD; glyphSortVariantLockOpen = $F0CCE;
  glyphSourceRepository = $F0CCF; glyphSourceRepositoryMultiple = $F0CD0; glyphSpa = $F0CD1;
  glyphSpaOutline = $F0CD2; glyphToasterOven = $F0CD3; glyphTruckCheck = $F0CD4;
  glyphTurnstile = $F0CD5; glyphTurnstileOutline = $F0CD6; glyphTurtle = $F0CD7;
  glyphUploadNetworkOutline = $F0CD8; glyphVibrateOff = $F0CD9; glyphWatchVibrateOff = $F0CDA;
  glyphArrowDownCircle = $F0CDB; glyphArrowDownCircleOutline = $F0CDC; glyphArrowLeftCircle = $F0CDD;
  glyphArrowLeftCircleOutline = $F0CDE; glyphArrowRightCircle = $F0CDF; glyphArrowRightCircleOutline = $F0CE0;
  glyphArrowUpCircle = $F0CE1; glyphArrowUpCircleOutline = $F0CE2; glyphAccountTie = $F0CE3;
  glyphAlertBoxOutline = $F0CE4; glyphAlertDecagramOutline = $F0CE5; glyphAlertOctagonOutline = $F0CE6;
  glyphAlertOctagramOutline = $F0CE7; glyphAmmunition = $F0CE8; glyphAccountMusicOutline = $F0CE9;
  glyphBeaker = $F0CEA; glyphBlender = $F0CEB; glyphBloodBag = $F0CEC;
  glyphCrossBolnisi = $F0CED; glyphBreadSlice = $F0CEE; glyphBreadSliceOutline = $F0CEF;
  glyphBriefcaseAccount = $F0CF0; glyphBriefcaseAccountOutline = $F0CF1; glyphBrightnessPercent = $F0CF2;
  glyphBullet = $F0CF3; glyphCashRegister = $F0CF4; glyphCrossCeltic = $F0CF5;
  glyphCrossOutline = $F0CF6; glyphClipboardAlertOutline = $F0CF7; glyphClipboardArrowLeftOutline = $F0CF8;
  glyphClipboardArrowRight = $F0CF9; glyphClipboardArrowRightOutline = $F0CFA; glyphContentSaveEdit = $F0CFB;
  glyphContentSaveEditOutline = $F0CFC; glyphCursorDefaultClick = $F0CFD; glyphCursorDefaultClickOutline = $F0CFE;
  glyphDatabaseSync = $F0CFF; glyphDatabaseRemove = $F0D00; glyphDatabaseSettings = $F0D01;
  glyphDramaMasks = $F0D02; glyphEmailBox = $F0D03; glyphEyeCheck = $F0D04;
  glyphEyeCheckOutline = $F0D05; glyphFastForward30 = $F0D06; glyphOrderAlphabeticalDescending = $F0D07;
  glyphFlowerPoppy = $F0D08; glyphFolderPound = $F0D09; glyphFolderPoundOutline = $F0D0A;
  glyphFolderSync = $F0D0B; glyphFolderSyncOutline = $F0D0C; glyphFormatListNumberedRtl = $F0D0D;
  glyphFormatTextWrappingClip = $F0D0E; glyphFormatTextWrappingOverflow = $F0D0F; glyphFormatTextWrappingWrap = $F0D10;
  glyphFormatTextbox = $F0D11; glyphFountainPen = $F0D12; glyphFountainPenTip = $F0D13;
  glyphHeartBrokenOutline = $F0D14; glyphHomeCity = $F0D15; glyphHomeCityOutline = $F0D16;
  glyphHubspot = $F0D17; glyphFilmstripBoxMultiple = $F0D18; glyphPlayBoxMultiple = $F0D19;
  glyphLinkBox = $F0D1A; glyphLinkBoxOutline = $F0D1B; glyphLinkBoxVariant = $F0D1C;
  glyphLinkBoxVariantOutline = $F0D1D; glyphMapClock = $F0D1E; glyphMapClockOutline = $F0D1F;
  glyphMapMarkerPath = $F0D20; glyphMotherNurse = $F0D21; glyphMicrosoftOutlook = $F0D22;
  glyphPerspectiveLess = $F0D23; glyphPerspectiveMore = $F0D24; glyphPodium = $F0D25;
  glyphPodiumBronze = $F0D26; glyphPodiumGold = $F0D27; glyphPodiumSilver = $F0D28;
  glyphQuora = $F0D29; glyphRewind10 = $F0D2A; glyphRollerSkate = $F0D2B;
  glyphRollerblade = $F0D2C; glyphLanguageRuby = $F0D2D; glyphSack = $F0D2E;
  glyphSackPercent = $F0D2F; glyphSafetyGoggles = $F0D30; glyphSelectColor = $F0D31;
  glyphSelectionEllipse = $F0D32; glyphShieldLinkVariant = $F0D33; glyphShieldLinkVariantOutline = $F0D34;
  glyphSkate = $F0D35; glyphSkewLess = $F0D36; glyphSkewMore = $F0D37;
  glyphSpeakerMultiple = $F0D38; glyphStamper = $F0D39; glyphTank = $F0D3A;
  glyphTortoise = $F0D3B; glyphTransitConnection = $F0D3C; glyphTransitConnectionVariant = $F0D3D;
  glyphTransmissionTower = $F0D3E; glyphWeightGram = $F0D3F; glyphYoutubeSubscription = $F0D40;
  glyphZigbee = $F0D41; glyphEmailAlertOutline = $F0D42; glyphAirFilter = $F0D43;
  glyphAirPurifier = $F0D44; glyphAndroidMessages = $F0D45; glyphAppsBox = $F0D46;
  glyphAtm = $F0D47; glyphAxis = $F0D48; glyphAxisArrow = $F0D49;
  glyphAxisArrowLock = $F0D4A; glyphAxisLock = $F0D4B; glyphAxisXArrow = $F0D4C;
  glyphAxisXArrowLock = $F0D4D; glyphAxisXRotateClockwise = $F0D4E; glyphAxisXRotateCounterclockwise = $F0D4F;
  glyphAxisXYArrowLock = $F0D50; glyphAxisYArrow = $F0D51; glyphAxisYArrowLock = $F0D52;
  glyphAxisYRotateClockwise = $F0D53; glyphAxisYRotateCounterclockwise = $F0D54; glyphAxisZArrow = $F0D55;
  glyphAxisZArrowLock = $F0D56; glyphAxisZRotateClockwise = $F0D57; glyphAxisZRotateCounterclockwise = $F0D58;
  glyphBellAlert = $F0D59; glyphBellCircle = $F0D5A; glyphBellCircleOutline = $F0D5B;
  glyphCalendarMinus = $F0D5C; glyphCameraOutline = $F0D5D; glyphCarBrakeHold = $F0D5E;
  glyphCarBrakeParking = $F0D5F; glyphCarCruiseControl = $F0D60; glyphCarDefrostFront = $F0D61;
  glyphCarDefrostRear = $F0D62; glyphCarParkingLights = $F0D63; glyphCarTractionControl = $F0D64;
  glyphBagCarryOnCheck = $F0D65; glyphCartArrowDown = $F0D66; glyphCartArrowUp = $F0D67;
  glyphCartMinus = $F0D68; glyphCartRemove = $F0D69; glyphContactlessPayment = $F0D6A;
  glyphCreativeCommons = $F0D6B; glyphCreditCardWirelessOutline = $F0D6C; glyphCricket = $F0D6D;
  glyphDevTo = $F0D6E; glyphDomainOff = $F0D6F; glyphFaceAgent = $F0D70;
  glyphFastForward10 = $F0D71; glyphFlare = $F0D72; glyphFormatTextRotationDown = $F0D73;
  glyphFormatTextRotationNone = $F0D74; glyphForwardburger = $F0D75; glyphGestureSwipe = $F0D76;
  glyphGestureTapHold = $F0D77; glyphFileGifBox = $F0D78; glyphGoKart = $F0D79;
  glyphGoKartTrack = $F0D7A; glyphGoodreads = $F0D7B; glyphGrain = $F0D7C;
  glyphHdr = $F0D7D; glyphHdrOff = $F0D7E; glyphHiking = $F0D7F;
  glyphHomeFloor1 = $F0D80; glyphHomeFloor2 = $F0D81; glyphHomeFloor3 = $F0D82;
  glyphHomeFloorA = $F0D83; glyphHomeFloorB = $F0D84; glyphHomeFloorG = $F0D85;
  glyphHomeFloorL = $F0D86; glyphKabaddi = $F0D87; glyphMailboxOpen = $F0D88;
  glyphMailboxOpenOutline = $F0D89; glyphMailboxOpenUp = $F0D8A; glyphMailboxOpenUpOutline = $F0D8B;
  glyphMailboxOutline = $F0D8C; glyphMailboxUp = $F0D8D; glyphMailboxUpOutline = $F0D8E;
  glyphMixedMartialArts = $F0D8F; glyphMonitorOff = $F0D90; glyphMotionSensor = $F0D91;
  glyphPointOfSale = $F0D92; glyphRacingHelmet = $F0D93; glyphRacquetball = $F0D94;
  glyphRestartOff = $F0D95; glyphRewind30 = $F0D96; glyphRoomServiceOutline = $F0D97;
  glyphRotateOrbit = $F0D98; glyphRugby = $F0D99; glyphShieldSearch = $F0D9A;
  glyphSolarPanel = $F0D9B; glyphSolarPanelLarge = $F0D9C; glyphSubwayAlertVariant = $F0D9D;
  glyphTea = $F0D9E; glyphTeaOutline = $F0D9F; glyphTennis = $F0DA0;
  glyphTransferDown = $F0DA1; glyphTransferLeft = $F0DA2; glyphTransferUp = $F0DA3;
  glyphTrophyBroken = $F0DA4; glyphWindTurbine = $F0DA5; glyphWiperWash = $F0DA6;
  glyphBadgeAccount = $F0DA7; glyphBadgeAccountAlert = $F0DA8; glyphBadgeAccountAlertOutline = $F0DA9;
  glyphBadgeAccountOutline = $F0DAA; glyphCardAccountDetailsOutline = $F0DAB; glyphAirHorn = $F0DAC;
  glyphApplicationExport = $F0DAD; glyphApplicationImport = $F0DAE; glyphBandage = $F0DAF;
  glyphBankMinus = $F0DB0; glyphBankPlus = $F0DB1; glyphBankRemove = $F0DB2;
  glyphBolt = $F0DB3; glyphBugle = $F0DB4; glyphCactus = $F0DB5;
  glyphCameraWireless = $F0DB6; glyphCameraWirelessOutline = $F0DB7; glyphCashMarker = $F0DB8;
  glyphChevronTripleDown = $F0DB9; glyphChevronTripleLeft = $F0DBA; glyphChevronTripleRight = $F0DBB;
  glyphChevronTripleUp = $F0DBC; glyphClosedCaptionOutline = $F0DBD; glyphCreditCardMarkerOutline = $F0DBE;
  glyphDivingFlippers = $F0DBF; glyphDivingHelmet = $F0DC0; glyphDivingScuba = $F0DC1;
  glyphDivingScubaFlag = $F0DC2; glyphDivingScubaTank = $F0DC3; glyphDivingScubaTankMultiple = $F0DC4;
  glyphDivingSnorkel = $F0DC5; glyphFileCancel = $F0DC6; glyphFileCancelOutline = $F0DC7;
  glyphFileDocumentEdit = $F0DC8; glyphFileDocumentEditOutline = $F0DC9; glyphFileEye = $F0DCA;
  glyphFileEyeOutline = $F0DCB; glyphFolderAlert = $F0DCC; glyphFolderAlertOutline = $F0DCD;
  glyphFolderEditOutline = $F0DCE; glyphFolderOpenOutline = $F0DCF; glyphFormatListBulletedSquare = $F0DD0;
  glyphGantryCrane = $F0DD1; glyphHomeFloor0 = $F0DD2; glyphHomeFloorNegative1 = $F0DD3;
  glyphHomeGroup = $F0DD4; glyphJabber = $F0DD5; glyphKeyOutline = $F0DD6;
  glyphLeak = $F0DD7; glyphLeakOff = $F0DD8; glyphMarkerCancel = $F0DD9;
  glyphMine = $F0DDA; glyphMonitorLock = $F0DDB; glyphMonitorStar = $F0DDC;
  glyphMovieOutline = $F0DDD; glyphMusicNotePlus = $F0DDE; glyphNail = $F0DDF;
  glyphOcarina = $F0DE0; glyphPassportBiometric = $F0DE1; glyphPenLock = $F0DE2;
  glyphPenMinus = $F0DE3; glyphPenOff = $F0DE4; glyphPenPlus = $F0DE5;
  glyphPenRemove = $F0DE6; glyphPencilLockOutline = $F0DE7; glyphPencilMinus = $F0DE8;
  glyphPencilMinusOutline = $F0DE9; glyphPencilOffOutline = $F0DEA; glyphPencilPlus = $F0DEB;
  glyphPencilPlusOutline = $F0DEC; glyphPencilRemove = $F0DED; glyphPencilRemoveOutline = $F0DEE;
  glyphPhoneOff = $F0DEF; glyphPhoneOutline = $F0DF0; glyphPiHole = $F0DF1;
  glyphPlaylistStar = $F0DF2; glyphScrewFlatTop = $F0DF3; glyphScrewLag = $F0DF4;
  glyphScrewMachineFlatTop = $F0DF5; glyphScrewMachineRoundTop = $F0DF6; glyphScrewRoundTop = $F0DF7;
  glyphSendCircle = $F0DF8; glyphSendCircleOutline = $F0DF9; glyphShoePrint = $F0DFA;
  glyphSignature = $F0DFB; glyphSignatureFreehand = $F0DFC; glyphSignatureImage = $F0DFD;
  glyphSignatureText = $F0DFE; glyphSlopeDownhill = $F0DFF; glyphSlopeUphill = $F0E00;
  glyphThermometerAlert = $F0E01; glyphThermometerChevronDown = $F0E02; glyphThermometerChevronUp = $F0E03;
  glyphThermometerMinus = $F0E04; glyphThermometerPlus = $F0E05; glyphTranslateOff = $F0E06;
  glyphUploadOutline = $F0E07; glyphVolumeVariantOff = $F0E08; glyphWallpaper = $F0E09;
  glyphWaterOutline = $F0E0A; glyphWifiStar = $F0E0B; glyphPaletteOutline = $F0E0C;
  glyphBadgeAccountHorizontal = $F0E0D; glyphBadgeAccountHorizontalOutline = $F0E0E; glyphAws = $F0E0F;
  glyphBagPersonal = $F0E10; glyphBagPersonalOff = $F0E11; glyphBagPersonalOffOutline = $F0E12;
  glyphBagPersonalOutline = $F0E13; glyphBiathlon = $F0E14; glyphBookmarkMultiple = $F0E15;
  glyphBookmarkMultipleOutline = $F0E16; glyphCalendarMonth = $F0E17; glyphCalendarMonthOutline = $F0E18;
  glyphCameraRetake = $F0E19; glyphCameraRetakeOutline = $F0E1A; glyphCarBack = $F0E1B;
  glyphCarOff = $F0E1C; glyphCastEducation = $F0E1D; glyphCheckBold = $F0E1E;
  glyphCheckUnderline = $F0E1F; glyphCheckUnderlineCircle = $F0E20; glyphCheckUnderlineCircleOutline = $F0E21;
  glyphCircularSaw = $F0E22; glyphComma = $F0E23; glyphCommaBoxOutline = $F0E24;
  glyphCommaCircle = $F0E25; glyphCommaCircleOutline = $F0E26; glyphContentSaveMove = $F0E27;
  glyphContentSaveMoveOutline = $F0E28; glyphFileCheckOutline = $F0E29; glyphFileMusicOutline = $F0E2A;
  glyphCommaBox = $F0E2B; glyphFileVideoOutline = $F0E2C; glyphFilePngBox = $F0E2D;
  glyphFireplace = $F0E2E; glyphFireplaceOff = $F0E2F; glyphFirework = $F0E30;
  glyphFormatColorHighlight = $F0E31; glyphFormatTextVariant = $F0E32; glyphGamepadCircle = $F0E33;
  glyphGamepadCircleDown = $F0E34; glyphGamepadCircleLeft = $F0E35; glyphGamepadCircleOutline = $F0E36;
  glyphGamepadCircleRight = $F0E37; glyphGamepadCircleUp = $F0E38; glyphGamepadDown = $F0E39;
  glyphGamepadLeft = $F0E3A; glyphGamepadRight = $F0E3B; glyphGamepadRound = $F0E3C;
  glyphGamepadRoundDown = $F0E3D; glyphGamepadRoundLeft = $F0E3E; glyphGamepadRoundOutline = $F0E3F;
  glyphGamepadRoundRight = $F0E40; glyphGamepadRoundUp = $F0E41; glyphGamepadUp = $F0E42;
  glyphGatsby = $F0E43; glyphGift = $F0E44; glyphGrill = $F0E45;
  glyphHandBackLeft = $F0E46; glyphHandBackRight = $F0E47; glyphHandSaw = $F0E48;
  glyphImageFrame = $F0E49; glyphInvertColorsOff = $F0E4A; glyphKeyboardOffOutline = $F0E4B;
  glyphLayersMinus = $F0E4C; glyphLayersPlus = $F0E4D; glyphLayersRemove = $F0E4E;
  glyphLightbulbOff = $F0E4F; glyphLightbulbOffOutline = $F0E50; glyphMonitorScreenshot = $F0E51;
  glyphIceCreamOff = $F0E52; glyphNfcSearchVariant = $F0E53; glyphNfcVariantOff = $F0E54;
  glyphNotebookMultiple = $F0E55; glyphHoopHouse = $F0E56; glyphPictureInPictureBottomRight = $F0E57;
  glyphPictureInPictureBottomRightOutline = $F0E58; glyphPictureInPictureTopRight = $F0E59; glyphPictureInPictureTopRightOutline = $F0E5A;
  glyphPrinter3dNozzle = $F0E5B; glyphPrinter3dNozzleOutline = $F0E5C; glyphPrinterOff = $F0E5D;
  glyphRectangle = $F0E5E; glyphRectangleOutline = $F0E5F; glyphRivet = $F0E60;
  glyphSawBlade = $F0E61; glyphSeed = $F0E62; glyphSeedOutline = $F0E63;
  glyphSignalDistanceVariant = $F0E64; glyphSpade = $F0E65; glyphSprout = $F0E66;
  glyphSproutOutline = $F0E67; glyphTableTennis = $F0E68; glyphTreeOutline = $F0E69;
  glyphViewComfy = $F0E6A; glyphViewCompact = $F0E6B; glyphViewCompactOutline = $F0E6C;
  glyphVuetify = $F0E6D; glyphWeatherCloudyArrowRight = $F0E6E; glyphMicrosoftXboxControllerMenu = $F0E6F;
  glyphMicrosoftXboxControllerView = $F0E70; glyphAlarmNote = $F0E71; glyphAlarmNoteOff = $F0E72;
  glyphArrowLeftRight = $F0E73; glyphArrowLeftRightBold = $F0E74; glyphArrowTopLeftBottomRight = $F0E75;
  glyphArrowTopLeftBottomRightBold = $F0E76; glyphArrowTopRightBottomLeft = $F0E77; glyphArrowTopRightBottomLeftBold = $F0E78;
  glyphArrowUpDown = $F0E79; glyphArrowUpDownBold = $F0E7A; glyphAtomVariant = $F0E7B;
  glyphBabyFace = $F0E7C; glyphBabyFaceOutline = $F0E7D; glyphBackspaceReverse = $F0E7E;
  glyphBackspaceReverseOutline = $F0E7F; glyphBankOutline = $F0E80; glyphBellAlertOutline = $F0E81;
  glyphBookPlay = $F0E82; glyphBookPlayOutline = $F0E83; glyphBookSearch = $F0E84;
  glyphBookSearchOutline = $F0E85; glyphBoomGate = $F0E86; glyphBoomGateAlert = $F0E87;
  glyphBoomGateAlertOutline = $F0E88; glyphBoomGateArrowDown = $F0E89; glyphBoomGateArrowDownOutline = $F0E8A;
  glyphBoomGateOutline = $F0E8B; glyphBoomGateArrowUp = $F0E8C; glyphBoomGateArrowUpOutline = $F0E8D;
  glyphCalendarSync = $F0E8E; glyphCalendarSyncOutline = $F0E8F; glyphCellphoneNfc = $F0E90;
  glyphChartAreasplineVariant = $F0E91; glyphChartScatterPlot = $F0E92; glyphChartTimelineVariant = $F0E93;
  glyphChartTree = $F0E94; glyphCircleDouble = $F0E95; glyphCircleExpand = $F0E96;
  glyphClockDigital = $F0E97; glyphCardAccountMailOutline = $F0E98; glyphCardAccountPhone = $F0E99;
  glyphCardAccountPhoneOutline = $F0E9A; glyphAccountCowboyHat = $F0E9B; glyphCurrencyRial = $F0E9C;
  glyphDeleteEmptyOutline = $F0E9D; glyphDolly = $F0E9E; glyphElectricSwitch = $F0E9F;
  glyphEllipse = $F0EA0; glyphEllipseOutline = $F0EA1; glyphEqualizer = $F0EA2;
  glyphEqualizerOutline = $F0EA3; glyphFerrisWheel = $F0EA4; glyphFileDelimitedOutline = $F0EA5;
  glyphTextBoxCheck = $F0EA6; glyphTextBoxCheckOutline = $F0EA7; glyphTextBoxMinus = $F0EA8;
  glyphTextBoxMinusOutline = $F0EA9; glyphTextBoxPlus = $F0EAA; glyphTextBoxPlusOutline = $F0EAB;
  glyphTextBoxRemove = $F0EAC; glyphTextBoxRemoveOutline = $F0EAD; glyphTextBoxSearch = $F0EAE;
  glyphTextBoxSearchOutline = $F0EAF; glyphFileImageOutline = $F0EB0; glyphFingerprintOff = $F0EB1;
  glyphFormatListBulletedTriangle = $F0EB2; glyphFormatOverline = $F0EB3; glyphFrequentlyAskedQuestions = $F0EB4;
  glyphGamepadSquare = $F0EB5; glyphGamepadSquareOutline = $F0EB6; glyphGamepadVariantOutline = $F0EB7;
  glyphGasStationOutline = $F0EB8; glyphGooglePodcast = $F0EB9; glyphHomeAnalytics = $F0EBA;
  glyphMail = $F0EBB; glyphMapCheck = $F0EBC; glyphMapCheckOutline = $F0EBD;
  glyphRulerSquareCompass = $F0EBE; glyphNotebookOutline = $F0EBF; glyphPenguin = $F0EC0;
  glyphRadioactiveOff = $F0EC1; glyphRecordCircle = $F0EC2; glyphRecordCircleOutline = $F0EC3;
  glyphRemoteOff = $F0EC4; glyphRemoteTv = $F0EC5; glyphRemoteTvOff = $F0EC6;
  glyphRotate3d = $F0EC7; glyphSailBoat = $F0EC8; glyphScatterPlot = $F0EC9;
  glyphScatterPlotOutline = $F0ECA; glyphSegment = $F0ECB; glyphShieldAlert = $F0ECC;
  glyphShieldAlertOutline = $F0ECD; glyphTabletDashboard = $F0ECE; glyphTelevisionPlay = $F0ECF;
  glyphUnicode = $F0ED0; glyphVideo3dVariant = $F0ED1; glyphVideoWireless = $F0ED2;
  glyphVideoWirelessOutline = $F0ED3; glyphAccountVoiceOff = $F0ED4; glyphBacteria = $F0ED5;
  glyphBacteriaOutline = $F0ED6; glyphCalendarAccount = $F0ED7; glyphCalendarAccountOutline = $F0ED8;
  glyphCalendarWeekend = $F0ED9; glyphCalendarWeekendOutline = $F0EDA; glyphCameraPlus = $F0EDB;
  glyphCameraPlusOutline = $F0EDC; glyphCampfire = $F0EDD; glyphChatOutline = $F0EDE;
  glyphCpu32Bit = $F0EDF; glyphCpu64Bit = $F0EE0; glyphCreditCardClock = $F0EE1;
  glyphCreditCardClockOutline = $F0EE2; glyphEmailEdit = $F0EE3; glyphEmailEditOutline = $F0EE4;
  glyphEmailMinus = $F0EE5; glyphEmailMinusOutline = $F0EE6; glyphEmailMultiple = $F0EE7;
  glyphEmailMultipleOutline = $F0EE8; glyphEmailOpenMultiple = $F0EE9; glyphEmailOpenMultipleOutline = $F0EEA;
  glyphFileCad = $F0EEB; glyphFileCadBox = $F0EEC; glyphFilePlusOutline = $F0EED;
  glyphFilterMinus = $F0EEE; glyphFilterMinusOutline = $F0EEF; glyphFilterPlus = $F0EF0;
  glyphFilterPlusOutline = $F0EF1; glyphFireExtinguisher = $F0EF2; glyphFishbowl = $F0EF3;
  glyphFishbowlOutline = $F0EF4; glyphFitToPage = $F0EF5; glyphFitToPageOutline = $F0EF6;
  glyphFlashAlert = $F0EF7; glyphFlashAlertOutline = $F0EF8; glyphHeartFlash = $F0EF9;
  glyphHomeFlood = $F0EFA; glyphHumanMaleHeight = $F0EFB; glyphHumanMaleHeightVariant = $F0EFC;
  glyphIcePop = $F0EFD; glyphIdentifier = $F0EFE; glyphImageFilterCenterFocusStrong = $F0EFF;
  glyphImageFilterCenterFocusStrongOutline = $F0F00; glyphJellyfish = $F0F01; glyphJellyfishOutline = $F0F02;
  glyphLasso = $F0F03; glyphMusicBoxMultipleOutline = $F0F04; glyphMapMarkerAlert = $F0F05;
  glyphMapMarkerAlertOutline = $F0F06; glyphMapMarkerQuestion = $F0F07; glyphMapMarkerQuestionOutline = $F0F08;
  glyphMapMarkerRemove = $F0F09; glyphMapMarkerRemoveVariant = $F0F0A; glyphNecklace = $F0F0B;
  glyphNewspaperMinus = $F0F0C; glyphNewspaperPlus = $F0F0D; glyphNumeric0BoxMultiple = $F0F0E;
  glyphNumeric1BoxMultiple = $F0F0F; glyphNumeric2BoxMultiple = $F0F10; glyphNumeric3BoxMultiple = $F0F11;
  glyphNumeric4BoxMultiple = $F0F12; glyphNumeric5BoxMultiple = $F0F13; glyphNumeric6BoxMultiple = $F0F14;
  glyphNumeric7BoxMultiple = $F0F15; glyphNumeric8BoxMultiple = $F0F16; glyphNumeric9BoxMultiple = $F0F17;
  glyphNumeric9PlusBoxMultiple = $F0F18; glyphOilLamp = $F0F19; glyphPhoneAlert = $F0F1A;
  glyphPlayOutline = $F0F1B; glyphPurse = $F0F1C; glyphPurseOutline = $F0F1D;
  glyphRailroadLight = $F0F1E; glyphReplyAllOutline = $F0F1F; glyphReplyOutline = $F0F20;
  glyphRssOff = $F0F21; glyphSelectionEllipseArrowInside = $F0F22; glyphShareOff = $F0F23;
  glyphShareOffOutline = $F0F24; glyphSkipBackwardOutline = $F0F25; glyphSkipForwardOutline = $F0F26;
  glyphSkipNextOutline = $F0F27; glyphSkipPreviousOutline = $F0F28; glyphSnowflakeAlert = $F0F29;
  glyphSnowflakeVariant = $F0F2A; glyphStretchToPage = $F0F2B; glyphStretchToPageOutline = $F0F2C;
  glyphTypewriter = $F0F2D; glyphWave = $F0F2E; glyphWeatherCloudyAlert = $F0F2F;
  glyphWeatherHazy = $F0F30; glyphWeatherNightPartlyCloudy = $F0F31; glyphWeatherPartlyLightning = $F0F32;
  glyphWeatherPartlyRainy = $F0F33; glyphWeatherPartlySnowy = $F0F34; glyphWeatherPartlySnowyRainy = $F0F35;
  glyphWeatherSnowyHeavy = $F0F36; glyphWeatherSunnyAlert = $F0F37; glyphWeatherTornado = $F0F38;
  glyphBabyBottle = $F0F39; glyphBabyBottleOutline = $F0F3A; glyphBagCarryOn = $F0F3B;
  glyphBagCarryOnOff = $F0F3C; glyphBagChecked = $F0F3D; glyphBaguette = $F0F3E;
  glyphBusMultiple = $F0F3F; glyphCarShiftPattern = $F0F40; glyphCellphoneInformation = $F0F41;
  glyphContentSaveAlert = $F0F42; glyphContentSaveAlertOutline = $F0F43; glyphContentSaveAllOutline = $F0F44;
  glyphCrosshairsOff = $F0F45; glyphCupboard = $F0F46; glyphCupboardOutline = $F0F47;
  glyphChairRolling = $F0F48; glyphDraw = $F0F49; glyphDresser = $F0F4A;
  glyphDresserOutline = $F0F4B; glyphEmoticonFrown = $F0F4C; glyphEmoticonFrownOutline = $F0F4D;
  glyphFocusAuto = $F0F4E; glyphFocusField = $F0F4F; glyphFocusFieldHorizontal = $F0F50;
  glyphFocusFieldVertical = $F0F51; glyphFootPrint = $F0F52; glyphHandball = $F0F53;
  glyphHomeThermometer = $F0F54; glyphHomeThermometerOutline = $F0F55; glyphKettleOutline = $F0F56;
  glyphLatitude = $F0F57; glyphLayersTriple = $F0F58; glyphLayersTripleOutline = $F0F59;
  glyphLongitude = $F0F5A; glyphLanguageMarkdownOutline = $F0F5B; glyphMerge = $F0F5C;
  glyphMiddleware = $F0F5D; glyphMiddlewareOutline = $F0F5E; glyphMonitorSpeaker = $F0F5F;
  glyphMonitorSpeakerOff = $F0F60; glyphMoonFirstQuarter = $F0F61; glyphMoonFull = $F0F62;
  glyphMoonLastQuarter = $F0F63; glyphMoonNew = $F0F64; glyphMoonWaningCrescent = $F0F65;
  glyphMoonWaningGibbous = $F0F66; glyphMoonWaxingCrescent = $F0F67; glyphMoonWaxingGibbous = $F0F68;
  glyphMusicAccidentalDoubleFlat = $F0F69; glyphMusicAccidentalDoubleSharp = $F0F6A; glyphMusicAccidentalFlat = $F0F6B;
  glyphMusicAccidentalNatural = $F0F6C; glyphMusicAccidentalSharp = $F0F6D; glyphMusicClefAlto = $F0F6E;
  glyphMusicClefBass = $F0F6F; glyphMusicClefTreble = $F0F70; glyphMusicNoteEighthDotted = $F0F71;
  glyphMusicNoteHalfDotted = $F0F72; glyphMusicNoteOffOutline = $F0F73; glyphMusicNoteOutline = $F0F74;
  glyphMusicNoteQuarterDotted = $F0F75; glyphMusicNoteSixteenthDotted = $F0F76; glyphMusicNoteWholeDotted = $F0F77;
  glyphMusicRestEighth = $F0F78; glyphMusicRestHalf = $F0F79; glyphMusicRestQuarter = $F0F7A;
  glyphMusicRestSixteenth = $F0F7B; glyphMusicRestWhole = $F0F7C; glyphNumeric10Box = $F0F7D;
  glyphNumeric10BoxOutline = $F0F7E; glyphPageLayoutHeaderFooter = $F0F7F; glyphPatioHeater = $F0F80;
  glyphWarehouse = $F0F81; glyphSelectGroup = $F0F82; glyphShieldCar = $F0F83;
  glyphShoppingSearch = $F0F84; glyphSpeedometerMedium = $F0F85; glyphSpeedometerSlow = $F0F86;
  glyphTableLargePlus = $F0F87; glyphTableLargeRemove = $F0F88; glyphTelevisionPause = $F0F89;
  glyphTelevisionStop = $F0F8A; glyphTransitDetour = $F0F8B; glyphVideoInputScart = $F0F8C;
  glyphViewGridPlus = $F0F8D; glyphWalletPlus = $F0F8E; glyphWalletPlusOutline = $F0F8F;
  glyphWardrobe = $F0F90; glyphWardrobeOutline = $F0F91; glyphWaterBoiler = $F0F92;
  glyphWaterPumpOff = $F0F93; glyphWebBox = $F0F94; glyphTimelineAlert = $F0F95;
  glyphTimelinePlus = $F0F96; glyphTimelinePlusOutline = $F0F97; glyphTimelineAlertOutline = $F0F98;
  glyphTimelineHelp = $F0F99; glyphTimelineHelpOutline = $F0F9A; glyphHomeExportOutline = $F0F9B;
  glyphHomeImportOutline = $F0F9C; glyphAccountFilterOutline = $F0F9D; glyphApproximatelyEqual = $F0F9E;
  glyphApproximatelyEqualBox = $F0F9F; glyphBabyCarriageOff = $F0FA0; glyphBee = $F0FA1;
  glyphBeeFlower = $F0FA2; glyphCarChildSeat = $F0FA3; glyphCarSeat = $F0FA4;
  glyphCarSeatCooler = $F0FA5; glyphCarSeatHeater = $F0FA6; glyphChartBellCurveCumulative = $F0FA7;
  glyphClockCheck = $F0FA8; glyphClockCheckOutline = $F0FA9; glyphCoffeeOff = $F0FAA;
  glyphCoffeeOffOutline = $F0FAB; glyphCreditCardMinus = $F0FAC; glyphCreditCardMinusOutline = $F0FAD;
  glyphCreditCardRemove = $F0FAE; glyphCreditCardRemoveOutline = $F0FAF; glyphDevices = $F0FB0;
  glyphEmailNewsletter = $F0FB1; glyphExpansionCardVariant = $F0FB2; glyphPowerSocketCh = $F0FB3;
  glyphFileSwap = $F0FB4; glyphFileSwapOutline = $F0FB5; glyphFolderSwap = $F0FB6;
  glyphFolderSwapOutline = $F0FB7; glyphFormatLetterEndsWith = $F0FB8; glyphFormatLetterMatches = $F0FB9;
  glyphFormatLetterStartsWith = $F0FBA; glyphFormatTextRotationAngleDown = $F0FBB; glyphFormatTextRotationAngleUp = $F0FBC;
  glyphFormatTextRotationDownVertical = $F0FBD; glyphFormatTextRotationUp = $F0FBE; glyphFormatTextRotationVertical = $F0FBF;
  glyphIdCard = $F0FC0; glyphImageAutoAdjust = $F0FC1; glyphKeyWireless = $F0FC2;
  glyphLicense = $F0FC3; glyphLocationEnter = $F0FC4; glyphLocationExit = $F0FC5;
  glyphLockOpenVariant = $F0FC6; glyphLockOpenVariantOutline = $F0FC7; glyphMathIntegral = $F0FC8;
  glyphMathIntegralBox = $F0FC9; glyphMathNorm = $F0FCA; glyphMathNormBox = $F0FCB;
  glyphMessageLock = $F0FCC; glyphMessageTextLock = $F0FCD; glyphMovieOpen = $F0FCE;
  glyphMovieOpenOutline = $F0FCF; glyphBedQueen = $F0FD0; glyphBedKingOutline = $F0FD1;
  glyphBedKing = $F0FD2; glyphBedDoubleOutline = $F0FD3; glyphBedDouble = $F0FD4;
  glyphMicrosoftAzureDevops = $F0FD5; glyphArmFlexOutline = $F0FD6; glyphArmFlex = $F0FD7;
  glyphProtocol = $F0FD8; glyphSealVariant = $F0FD9; glyphSelectPlace = $F0FDA;
  glyphBedQueenOutline = $F0FDB; glyphSignDirectionPlus = $F0FDC; glyphSignDirectionRemove = $F0FDD;
  glyphSilverwareClean = $F0FDE; glyphSlashForward = $F0FDF; glyphSlashForwardBox = $F0FE0;
  glyphSwapHorizontalCircle = $F0FE1; glyphSwapHorizontalCircleOutline = $F0FE2; glyphSwapVerticalCircle = $F0FE3;
  glyphSwapVerticalCircleOutline = $F0FE4; glyphTankerTruck = $F0FE5; glyphTextureBox = $F0FE6;
  glyphTramSide = $F0FE7; glyphVectorLink = $F0FE8; glyphNumeric10 = $F0FE9;
  glyphNumeric10BoxMultiple = $F0FEA; glyphNumeric10BoxMultipleOutline = $F0FEB; glyphNumeric10Circle = $F0FEC;
  glyphNumeric10CircleOutline = $F0FED; glyphNumeric9Plus = $F0FEE; glyphCreditCard = $F0FEF;
  glyphCreditCardMultiple = $F0FF0; glyphCreditCardOff = $F0FF1; glyphCreditCardPlus = $F0FF2;
  glyphCreditCardRefund = $F0FF3; glyphCreditCardScan = $F0FF4; glyphCreditCardSettings = $F0FF5;
  glyphHospital = $F0FF6; glyphHospitalBoxOutline = $F0FF7; glyphOilTemperature = $F0FF8;
  glyphStadium = $F0FF9; glyphZipBoxOutline = $F0FFA; glyphAccountEditOutline = $F0FFB;
  glyphPeanut = $F0FFC; glyphPeanutOff = $F0FFD; glyphPeanutOutline = $F0FFE;
  glyphPeanutOffOutline = $F0FFF; glyphSignDirectionMinus = $F1000; glyphNewspaperVariant = $F1001;
  glyphNewspaperVariantMultiple = $F1002; glyphNewspaperVariantMultipleOutline = $F1003; glyphNewspaperVariantOutline = $F1004;
  glyphOverscan = $F1005; glyphPigVariant = $F1006; glyphPiggyBank = $F1007;
  glyphPost = $F1008; glyphPostOutline = $F1009; glyphAccountBoxMultipleOutline = $F100A;
  glyphAirballoonOutline = $F100B; glyphAlphabeticalOff = $F100C; glyphAlphabeticalVariant = $F100D;
  glyphAlphabeticalVariantOff = $F100E; glyphApacheKafka = $F100F; glyphBillboard = $F1010;
  glyphBlindsOpen = $F1011; glyphBusStop = $F1012; glyphBusStopCovered = $F1013;
  glyphBusStopUncovered = $F1014; glyphCar2Plus = $F1015; glyphCar3Plus = $F1016;
  glyphCarBrakeRetarder = $F1017; glyphCarClutch = $F1018; glyphCarCoolantLevel = $F1019;
  glyphCarTurbocharger = $F101A; glyphCarWindshield = $F101B; glyphCarWindshieldOutline = $F101C;
  glyphCardsDiamondOutline = $F101D; glyphCastAudio = $F101E; glyphCellphonePlay = $F101F;
  glyphCoachLamp = $F1020; glyphCommentQuote = $F1021; glyphCommentQuoteOutline = $F1022;
  glyphDominoMask = $F1023; glyphElectronFramework = $F1024; glyphExcavator = $F1025;
  glyphEyeMinus = $F1026; glyphEyeMinusOutline = $F1027; glyphFileAccountOutline = $F1028;
  glyphFileChartOutline = $F1029; glyphFileCloudOutline = $F102A; glyphFileCodeOutline = $F102B;
  glyphFileExcelBoxOutline = $F102C; glyphFileExcelOutline = $F102D; glyphFileExportOutline = $F102E;
  glyphFileImportOutline = $F102F; glyphFileLockOutline = $F1030; glyphFileMoveOutline = $F1031;
  glyphFileMultipleOutline = $F1032; glyphFilePercentOutline = $F1033; glyphFilePowerpointBoxOutline = $F1034;
  glyphFilePowerpointOutline = $F1035; glyphFileQuestionOutline = $F1036; glyphFileRemoveOutline = $F1037;
  glyphFileRestoreOutline = $F1038; glyphFileSendOutline = $F1039; glyphFileStar = $F103A;
  glyphFileStarOutline = $F103B; glyphFileUndoOutline = $F103C; glyphFileWordBoxOutline = $F103D;
  glyphFileWordOutline = $F103E; glyphFilterVariantRemove = $F103F; glyphFloorLampDual = $F1040;
  glyphFloorLampTorchiereVariant = $F1041; glyphFruitCherries = $F1042; glyphFruitCitrus = $F1043;
  glyphFruitGrapes = $F1044; glyphFruitGrapesOutline = $F1045; glyphFruitPineapple = $F1046;
  glyphFruitWatermelon = $F1047; glyphGoogleMyBusiness = $F1048; glyphGraph = $F1049;
  glyphGraphOutline = $F104A; glyphHarddiskPlus = $F104B; glyphHarddiskRemove = $F104C;
  glyphHomeCircleOutline = $F104D; glyphInstrumentTriangle = $F104E; glyphIsland = $F104F;
  glyphKeyboardSpace = $F1050; glyphLedStripVariant = $F1051; glyphNumericNegative1 = $F1052;
  glyphOilLevel = $F1053; glyphOutdoorLamp = $F1054; glyphPalmTree = $F1055;
  glyphPartyPopper = $F1056; glyphPrinterPos = $F1057; glyphRobber = $F1058;
  glyphRoutesClock = $F1059; glyphScaleOff = $F105A; glyphCogTransfer = $F105B;
  glyphCogTransferOutline = $F105C; glyphShieldSun = $F105D; glyphShieldSunOutline = $F105E;
  glyphSprinkler = $F105F; glyphSprinklerVariant = $F1060; glyphTableChair = $F1061;
  glyphTerraform = $F1062; glyphToaster = $F1063; glyphTools = $F1064;
  glyphTransfer = $F1065; glyphValve = $F1066; glyphValveClosed = $F1067;
  glyphValveOpen = $F1068; glyphVideoCheck = $F1069; glyphVideoCheckOutline = $F106A;
  glyphWaterWell = $F106B; glyphWaterWellOutline = $F106C; glyphBedSingle = $F106D;
  glyphBedSingleOutline = $F106E; glyphBookInformationVariant = $F106F; glyphBottleSoda = $F1070;
  glyphBottleSodaClassic = $F1071; glyphBottleSodaOutline = $F1072; glyphCalendarBlankMultiple = $F1073;
  glyphCardSearch = $F1074; glyphCardSearchOutline = $F1075; glyphFaceWomanProfile = $F1076;
  glyphFaceWoman = $F1077; glyphFaceWomanOutline = $F1078; glyphFileSettings = $F1079;
  glyphFileSettingsOutline = $F107A; glyphFileCog = $F107B; glyphFileCogOutline = $F107C;
  glyphFolderSettings = $F107D; glyphFolderSettingsOutline = $F107E; glyphFolderCog = $F107F;
  glyphFolderCogOutline = $F1080; glyphFuriganaHorizontal = $F1081; glyphFuriganaVertical = $F1082;
  glyphGolfTee = $F1083; glyphLungs = $F1084; glyphMathLog = $F1085;
  glyphMoped = $F1086; glyphRouterNetwork = $F1087; glyphRomanNumeral2 = $F1089;
  glyphRomanNumeral3 = $F108A; glyphRomanNumeral4 = $F108B; glyphRomanNumeral6 = $F108D;
  glyphRomanNumeral7 = $F108E; glyphRomanNumeral8 = $F108F; glyphRomanNumeral9 = $F1090;
  glyphSolderingIron = $F1092; glyphStomach = $F1093; glyphTableEye = $F1094;
  glyphFormTextarea = $F1095; glyphTrumpet = $F1096; glyphAccountCash = $F1097;
  glyphAccountCashOutline = $F1098; glyphAirHumidifier = $F1099; glyphAnsible = $F109A;
  glyphApi = $F109B; glyphBicycle = $F109C; glyphCarDoorLock = $F109D;
  glyphCoatRack = $F109E; glyphCoffeeMaker = $F109F; glyphWebMinus = $F10A0;
  glyphDecimal = $F10A1; glyphDecimalComma = $F10A2; glyphDecimalCommaDecrease = $F10A3;
  glyphDecimalCommaIncrease = $F10A4; glyphDeleteAlert = $F10A5; glyphDeleteAlertOutline = $F10A6;
  glyphDeleteOff = $F10A7; glyphDeleteOffOutline = $F10A8; glyphDockBottom = $F10A9;
  glyphDockLeft = $F10AA; glyphDockRight = $F10AB; glyphDockWindow = $F10AC;
  glyphDomainPlus = $F10AD; glyphDomainRemove = $F10AE; glyphDoorClosedLock = $F10AF;
  glyphDownloadOff = $F10B0; glyphDownloadOffOutline = $F10B1; glyphFlagMinusOutline = $F10B2;
  glyphFlagPlusOutline = $F10B3; glyphFlagRemoveOutline = $F10B4; glyphFolderHome = $F10B5;
  glyphFolderHomeOutline = $F10B6; glyphFolderInformation = $F10B7; glyphFolderInformationOutline = $F10B8;
  glyphIvBag = $F10B9; glyphLinkLock = $F10BA; glyphMessagePlusOutline = $F10BB;
  glyphPhoneCancel = $F10BC; glyphSmartCard = $F10BD; glyphSmartCardOutline = $F10BE;
  glyphSmartCardReader = $F10BF; glyphSmartCardReaderOutline = $F10C0; glyphStorefrontOutline = $F10C1;
  glyphThermometerHigh = $F10C2; glyphThermometerLow = $F10C3; glyphUfo = $F10C4;
  glyphUfoOutline = $F10C5; glyphUploadOff = $F10C6; glyphUploadOffOutline = $F10C7;
  glyphAccountChildOutline = $F10C8; glyphAccountSettingsOutline = $F10C9; glyphAccountTieOutline = $F10CA;
  glyphAlienOutline = $F10CB; glyphBatteryAlertVariant = $F10CC; glyphBatteryAlertVariantOutline = $F10CD;
  glyphBeehiveOutline = $F10CE; glyphBoomerang = $F10CF; glyphBriefcaseClock = $F10D0;
  glyphBriefcaseClockOutline = $F10D1; glyphCellphoneMessageOff = $F10D2; glyphCircleOffOutline = $F10D3;
  glyphClipboardList = $F10D4; glyphClipboardListOutline = $F10D5; glyphCodeBracesBox = $F10D6;
  glyphCodeParenthesesBox = $F10D7; glyphConsolidate = $F10D8; glyphElectricSwitchClosed = $F10D9;
  glyphEmailReceive = $F10DA; glyphEmailReceiveOutline = $F10DB; glyphEmailSend = $F10DC;
  glyphEmailSendOutline = $F10DD; glyphEmoticonConfused = $F10DE; glyphEmoticonConfusedOutline = $F10DF;
  glyphEpsilon = $F10E0; glyphFileTableBox = $F10E1; glyphFileTableBoxMultiple = $F10E2;
  glyphFileTableBoxMultipleOutline = $F10E3; glyphFileTableBoxOutline = $F10E4; glyphFilterMenu = $F10E5;
  glyphFilterMenuOutline = $F10E6; glyphFlipHorizontal = $F10E7; glyphFlipVertical = $F10E8;
  glyphFolderDownloadOutline = $F10E9; glyphFolderHeart = $F10EA; glyphFolderHeartOutline = $F10EB;
  glyphFolderKeyOutline = $F10EC; glyphFolderUploadOutline = $F10ED; glyphGamma = $F10EE;
  glyphHairDryer = $F10EF; glyphHairDryerOutline = $F10F0; glyphHandHeart = $F10F1;
  glyphHexagonMultipleOutline = $F10F2; glyphHorizontalRotateClockwise = $F10F3; glyphHorizontalRotateCounterclockwise = $F10F4;
  glyphApplicationArray = $F10F5; glyphApplicationArrayOutline = $F10F6; glyphApplicationBraces = $F10F7;
  glyphApplicationBracesOutline = $F10F8; glyphApplicationParentheses = $F10F9; glyphApplicationParenthesesOutline = $F10FA;
  glyphApplicationVariable = $F10FB; glyphApplicationVariableOutline = $F10FC; glyphKhanda = $F10FD;
  glyphKubernetes = $F10FE; glyphLinkVariantMinus = $F10FF; glyphLinkVariantPlus = $F1100;
  glyphLinkVariantRemove = $F1101; glyphMapMarkerDown = $F1102; glyphMapMarkerUp = $F1103;
  glyphMonitorShimmer = $F1104; glyphNix = $F1105; glyphNuxt = $F1106;
  glyphPowerSocketDe = $F1107; glyphPowerSocketFr = $F1108; glyphPowerSocketJp = $F1109;
  glyphProgressClose = $F110A; glyphReloadAlert = $F110B; glyphRestartAlert = $F110C;
  glyphRestoreAlert = $F110D; glyphShaker = $F110E; glyphShakerOutline = $F110F;
  glyphTelevisionShimmer = $F1110; glyphVariableBox = $F1111; glyphFilterVariantMinus = $F1112;
  glyphFilterVariantPlus = $F1113; glyphSlotMachine = $F1114; glyphSlotMachineOutline = $F1115;
  glyphGlassMugVariant = $F1116; glyphClipboardFlowOutline = $F1117; glyphSignRealEstate = $F1118;
  glyphAntenna = $F1119; glyphCentos = $F111A; glyphRedhat = $F111B;
  glyphWindowShutter = $F111C; glyphWindowShutterAlert = $F111D; glyphWindowShutterOpen = $F111E;
  glyphBikeFast = $F111F; glyphVolumeSource = $F1120; glyphVolumeVibrate = $F1121;
  glyphMovieEdit = $F1122; glyphMovieEditOutline = $F1123; glyphMovieFilter = $F1124;
  glyphMovieFilterOutline = $F1125; glyphDiabetes = $F1126; glyphCursorDefaultGesture = $F1127;
  glyphCursorDefaultGestureOutline = $F1128; glyphToothbrush = $F1129; glyphToothbrushPaste = $F112A;
  glyphHomeRoof = $F112B; glyphToothbrushElectric = $F112C; glyphAccountSupervisorOutline = $F112D;
  glyphBottleTonic = $F112E; glyphBottleTonicOutline = $F112F; glyphBottleTonicPlus = $F1130;
  glyphBottleTonicPlusOutline = $F1131; glyphBottleTonicSkull = $F1132; glyphBottleTonicSkullOutline = $F1133;
  glyphCalendarArrowLeft = $F1134; glyphCalendarArrowRight = $F1135; glyphCrosshairsQuestion = $F1136;
  glyphFireHydrant = $F1137; glyphFireHydrantAlert = $F1138; glyphFireHydrantOff = $F1139;
  glyphOcr = $F113A; glyphShieldStar = $F113B; glyphShieldStarOutline = $F113C;
  glyphTextRecognition = $F113D; glyphHandcuffs = $F113E; glyphGenderMaleFemaleVariant = $F113F;
  glyphGenderNonBinary = $F1140; glyphMinusBoxMultiple = $F1141; glyphMinusBoxMultipleOutline = $F1142;
  glyphPlusBoxMultipleOutline = $F1143; glyphPencilBoxMultiple = $F1144; glyphPencilBoxMultipleOutline = $F1145;
  glyphPrinterCheck = $F1146; glyphSortVariantRemove = $F1147; glyphSortAlphabeticalAscendingVariant = $F1148;
  glyphSortAlphabeticalDescendingVariant = $F1149; glyphDice1Outline = $F114A; glyphDice2Outline = $F114B;
  glyphDice3Outline = $F114C; glyphDice4Outline = $F114D; glyphDice5Outline = $F114E;
  glyphDice6Outline = $F114F; glyphDiceD4 = $F1150; glyphDiceD6 = $F1151;
  glyphDiceD8 = $F1152; glyphDiceD10 = $F1153; glyphDiceD12 = $F1154;
  glyphDiceD20 = $F1155; glyphDiceMultipleOutline = $F1156; glyphPaperRoll = $F1157;
  glyphPaperRollOutline = $F1158; glyphHomeEdit = $F1159; glyphHomeEditOutline = $F115A;
  glyphArrowHorizontalLock = $F115B; glyphArrowVerticalLock = $F115C; glyphWeightLifter = $F115D;
  glyphAccountLock = $F115E; glyphAccountLockOutline = $F115F; glyphPasta = $F1160;
  glyphSendCheck = $F1161; glyphSendCheckOutline = $F1162; glyphSendClock = $F1163;
  glyphSendClockOutline = $F1164; glyphSendOutline = $F1165; glyphSendLockOutline = $F1166;
  glyphPoliceBadge = $F1167; glyphPoliceBadgeOutline = $F1168; glyphGateArrowRight = $F1169;
  glyphGateOpen = $F116A; glyphBellBadge = $F116B; glyphMessageImageOutline = $F116C;
  glyphMessageLockOutline = $F116D; glyphMessageMinus = $F116E; glyphMessageMinusOutline = $F116F;
  glyphMessageProcessingOutline = $F1170; glyphMessageSettingsOutline = $F1171; glyphMessageCogOutline = $F1172;
  glyphMessageTextClock = $F1173; glyphMessageTextClockOutline = $F1174; glyphMessageTextLockOutline = $F1175;
  glyphCheckboxBlankBadge = $F1176; glyphFileLink = $F1177; glyphFileLinkOutline = $F1178;
  glyphFilePhone = $F1179; glyphFilePhoneOutline = $F117A; glyphMeditation = $F117B;
  glyphYoga = $F117C; glyphLeek = $F117D; glyphNoodles = $F117E;
  glyphPoundBoxOutline = $F117F; glyphSchoolOutline = $F1180; glyphBasketOutline = $F1181;
  glyphPhoneInTalkOutline = $F1182; glyphBash = $F1183; glyphFileKey = $F1184;
  glyphFileKeyOutline = $F1185; glyphFileCertificate = $F1186; glyphFileCertificateOutline = $F1187;
  glyphCertificateOutline = $F1188; glyphCigar = $F1189; glyphGrillOutline = $F118A;
  glyphQrcodePlus = $F118B; glyphQrcodeMinus = $F118C; glyphQrcodeRemove = $F118D;
  glyphPhoneAlertOutline = $F118E; glyphPhoneBluetoothOutline = $F118F; glyphPhoneCancelOutline = $F1190;
  glyphPhoneForwardOutline = $F1191; glyphPhoneHangupOutline = $F1192; glyphPhoneIncomingOutline = $F1193;
  glyphPhoneLockOutline = $F1194; glyphPhoneLogOutline = $F1195; glyphPhoneMessage = $F1196;
  glyphPhoneMessageOutline = $F1197; glyphPhoneMinusOutline = $F1198; glyphPhoneOutgoingOutline = $F1199;
  glyphPhonePausedOutline = $F119A; glyphPhonePlusOutline = $F119B; glyphPhoneReturnOutline = $F119C;
  glyphPhoneSettingsOutline = $F119D; glyphKeyStar = $F119E; glyphKeyLink = $F119F;
  glyphShieldEdit = $F11A0; glyphShieldEditOutline = $F11A1; glyphShieldSync = $F11A2;
  glyphShieldSyncOutline = $F11A3; glyphGolfCart = $F11A4; glyphPhoneMissedOutline = $F11A5;
  glyphPhoneOffOutline = $F11A6; glyphFormatQuoteOpenOutline = $F11A7; glyphFormatQuoteCloseOutline = $F11A8;
  glyphPhoneCheck = $F11A9; glyphPhoneCheckOutline = $F11AA; glyphPhoneRing = $F11AB;
  glyphPhoneRingOutline = $F11AC; glyphShareCircle = $F11AD; glyphReplyCircle = $F11AE;
  glyphFridgeOff = $F11AF; glyphFridgeOffOutline = $F11B0; glyphFridgeAlert = $F11B1;
  glyphFridgeAlertOutline = $F11B2; glyphWaterBoilerAlert = $F11B3; glyphWaterBoilerOff = $F11B4;
  glyphAmplifierOff = $F11B5; glyphAudioVideoOff = $F11B6; glyphToasterOff = $F11B7;
  glyphDishwasherAlert = $F11B8; glyphDishwasherOff = $F11B9; glyphTumbleDryerAlert = $F11BA;
  glyphTumbleDryerOff = $F11BB; glyphWashingMachineAlert = $F11BC; glyphWashingMachineOff = $F11BD;
  glyphCarInfo = $F11BE; glyphCommentEdit = $F11BF; glyphPrinter3dNozzleAlert = $F11C0;
  glyphPrinter3dNozzleAlertOutline = $F11C1; glyphAlignHorizontalLeft = $F11C2; glyphAlignHorizontalCenter = $F11C3;
  glyphAlignHorizontalRight = $F11C4; glyphAlignVerticalBottom = $F11C5; glyphAlignVerticalCenter = $F11C6;
  glyphAlignVerticalTop = $F11C7; glyphDistributeHorizontalLeft = $F11C8; glyphDistributeHorizontalCenter = $F11C9;
  glyphDistributeHorizontalRight = $F11CA; glyphDistributeVerticalBottom = $F11CB; glyphDistributeVerticalCenter = $F11CC;
  glyphDistributeVerticalTop = $F11CD; glyphAlertRhombus = $F11CE; glyphAlertRhombusOutline = $F11CF;
  glyphCrownOutline = $F11D0; glyphImageOffOutline = $F11D1; glyphMovieSearch = $F11D2;
  glyphMovieSearchOutline = $F11D3; glyphRvTruck = $F11D4; glyphShoppingOutline = $F11D5;
  glyphStrategy = $F11D6; glyphNoteTextOutline = $F11D7; glyphViewAgendaOutline = $F11D8;
  glyphViewGridOutline = $F11D9; glyphViewGridPlusOutline = $F11DA; glyphWindowClosedVariant = $F11DB;
  glyphWindowOpenVariant = $F11DC; glyphCogClockwise = $F11DD; glyphCogCounterclockwise = $F11DE;
  glyphChartSankey = $F11DF; glyphChartSankeyVariant = $F11E0; glyphVanityLight = $F11E1;
  glyphRouter = $F11E2; glyphImageEdit = $F11E3; glyphImageEditOutline = $F11E4;
  glyphBellCheck = $F11E5; glyphBellCheckOutline = $F11E6; glyphFileEdit = $F11E7;
  glyphFileEditOutline = $F11E8; glyphHumanScooter = $F11E9; glyphSpider = $F11EA;
  glyphSpiderThread = $F11EB; glyphPlusThick = $F11EC; glyphAlertCircleCheck = $F11ED;
  glyphAlertCircleCheckOutline = $F11EE; glyphStateMachine = $F11EF; glyphUsbPort = $F11F0;
  glyphCloudLock = $F11F1; glyphCloudLockOutline = $F11F2; glyphRobotMowerOutline = $F11F3;
  glyphShareAll = $F11F4; glyphShareAllOutline = $F11F5; glyphGoogleCloud = $F11F6;
  glyphRobotMower = $F11F7; glyphFastForward5 = $F11F8; glyphRewind5 = $F11F9;
  glyphShapeOvalPlus = $F11FA; glyphTimelineClock = $F11FB; glyphTimelineClockOutline = $F11FC;
  glyphMirror = $F11FD; glyphAccountMultipleCheckOutline = $F11FE; glyphCardPlus = $F11FF;
  glyphCardPlusOutline = $F1200; glyphCheckerboardPlus = $F1201; glyphCheckerboardMinus = $F1202;
  glyphCheckerboardRemove = $F1203; glyphSelectSearch = $F1204; glyphSelectionSearch = $F1205;
  glyphLayersSearch = $F1206; glyphLayersSearchOutline = $F1207; glyphLightbulbCfl = $F1208;
  glyphLightbulbCflOff = $F1209; glyphAccountMultipleRemove = $F120A; glyphAccountMultipleRemoveOutline = $F120B;
  glyphMagnifyRemoveCursor = $F120C; glyphMagnifyRemoveOutline = $F120D; glyphArchiveOutline = $F120E;
  glyphBatteryHeart = $F120F; glyphBatteryHeartOutline = $F1210; glyphBatteryHeartVariant = $F1211;
  glyphBusMarker = $F1212; glyphChartMultiple = $F1213; glyphEmoticonLol = $F1214;
  glyphEmoticonLolOutline = $F1215; glyphFileSync = $F1216; glyphFileSyncOutline = $F1217;
  glyphHandshake = $F1218; glyphLanguageKotlin = $F1219; glyphLanguageFortran = $F121A;
  glyphOffer = $F121B; glyphRadioOff = $F121C; glyphTableHeadersEye = $F121D;
  glyphTableHeadersEyeOff = $F121E; glyphTagMinusOutline = $F121F; glyphTagOff = $F1220;
  glyphTagOffOutline = $F1221; glyphTagPlusOutline = $F1222; glyphTagRemoveOutline = $F1223;
  glyphTagText = $F1224; glyphVectorPolylineEdit = $F1225; glyphVectorPolylineMinus = $F1226;
  glyphVectorPolylinePlus = $F1227; glyphVectorPolylineRemove = $F1228; glyphBeakerAlert = $F1229;
  glyphBeakerAlertOutline = $F122A; glyphBeakerCheck = $F122B; glyphBeakerCheckOutline = $F122C;
  glyphBeakerMinus = $F122D; glyphBeakerMinusOutline = $F122E; glyphBeakerPlus = $F122F;
  glyphBeakerPlusOutline = $F1230; glyphBeakerQuestion = $F1231; glyphBeakerQuestionOutline = $F1232;
  glyphBeakerRemove = $F1233; glyphBeakerRemoveOutline = $F1234; glyphBicycleBasket = $F1235;
  glyphBarcodeOff = $F1236; glyphDigitalOcean = $F1237; glyphExclamationThick = $F1238;
  glyphDesk = $F1239; glyphFlaskEmptyMinus = $F123A; glyphFlaskEmptyMinusOutline = $F123B;
  glyphFlaskEmptyPlus = $F123C; glyphFlaskEmptyPlusOutline = $F123D; glyphFlaskEmptyRemove = $F123E;
  glyphFlaskEmptyRemoveOutline = $F123F; glyphFlaskMinus = $F1240; glyphFlaskMinusOutline = $F1241;
  glyphFlaskPlus = $F1242; glyphFlaskPlusOutline = $F1243; glyphFlaskRemove = $F1244;
  glyphFlaskRemoveOutline = $F1245; glyphFolderMoveOutline = $F1246; glyphHomeRemove = $F1247;
  glyphWebrtc = $F1248; glyphSeatPassenger = $F1249; glyphWebClock = $F124A;
  glyphFlaskRoundBottom = $F124B; glyphFlaskRoundBottomEmpty = $F124C; glyphFlaskRoundBottomEmptyOutline = $F124D;
  glyphFlaskRoundBottomOutline = $F124E; glyphGold = $F124F; glyphMessageStarOutline = $F1250;
  glyphHomeLightbulb = $F1251; glyphHomeLightbulbOutline = $F1252; glyphLightbulbGroup = $F1253;
  glyphLightbulbGroupOutline = $F1254; glyphLightbulbMultiple = $F1255; glyphLightbulbMultipleOutline = $F1256;
  glyphApiOff = $F1257; glyphAllergy = $F1258; glyphArchiveArrowDown = $F1259;
  glyphArchiveArrowDownOutline = $F125A; glyphArchiveArrowUp = $F125B; glyphArchiveArrowUpOutline = $F125C;
  glyphBatteryOff = $F125D; glyphBatteryOffOutline = $F125E; glyphBookshelf = $F125F;
  glyphCashMinus = $F1260; glyphCashPlus = $F1261; glyphCashRemove = $F1262;
  glyphClipboardCheckMultiple = $F1263; glyphClipboardCheckMultipleOutline = $F1264; glyphClipboardFile = $F1265;
  glyphClipboardFileOutline = $F1266; glyphClipboardMultiple = $F1267; glyphClipboardMultipleOutline = $F1268;
  glyphClipboardPlayMultiple = $F1269; glyphClipboardPlayMultipleOutline = $F126A; glyphClipboardTextMultiple = $F126B;
  glyphClipboardTextMultipleOutline = $F126C; glyphFolderMarker = $F126D; glyphFolderMarkerOutline = $F126E;
  glyphFormatListText = $F126F; glyphInboxArrowDownOutline = $F1270; glyphInboxArrowUpOutline = $F1271;
  glyphInboxFull = $F1272; glyphInboxFullOutline = $F1273; glyphInboxOutline = $F1274;
  glyphLightbulbCflSpiral = $F1275; glyphMagnifyScan = $F1276; glyphMapMarkerMultipleOutline = $F1277;
  glyphPercentOutline = $F1278; glyphPhoneClassicOff = $F1279; glyphPlayBox = $F127A;
  glyphAccountEyeOutline = $F127B; glyphSafeSquare = $F127C; glyphSafeSquareOutline = $F127D;
  glyphScoreboard = $F127E; glyphScoreboardOutline = $F127F; glyphSelectMarker = $F1280;
  glyphSelectMultiple = $F1281; glyphSelectMultipleMarker = $F1282; glyphSelectionMarker = $F1283;
  glyphSelectionMultipleMarker = $F1284; glyphSelectionMultiple = $F1285; glyphStarBoxMultiple = $F1286;
  glyphStarBoxMultipleOutline = $F1287; glyphToyBrick = $F1288; glyphToyBrickMarker = $F1289;
  glyphToyBrickMarkerOutline = $F128A; glyphToyBrickMinus = $F128B; glyphToyBrickMinusOutline = $F128C;
  glyphToyBrickOutline = $F128D; glyphToyBrickPlus = $F128E; glyphToyBrickPlusOutline = $F128F;
  glyphToyBrickRemove = $F1290; glyphToyBrickRemoveOutline = $F1291; glyphToyBrickSearch = $F1292;
  glyphToyBrickSearchOutline = $F1293; glyphTray = $F1294; glyphTrayAlert = $F1295;
  glyphTrayFull = $F1296; glyphTrayMinus = $F1297; glyphTrayPlus = $F1298;
  glyphTrayRemove = $F1299; glyphTruckCheckOutline = $F129A; glyphTruckDeliveryOutline = $F129B;
  glyphTruckFastOutline = $F129C; glyphTruckOutline = $F129D; glyphUsbFlashDrive = $F129E;
  glyphUsbFlashDriveOutline = $F129F; glyphWaterPolo = $F12A0; glyphBatteryLow = $F12A1;
  glyphBatteryMedium = $F12A2; glyphBatteryHigh = $F12A3; glyphBatteryChargingLow = $F12A4;
  glyphBatteryChargingMedium = $F12A5; glyphBatteryChargingHigh = $F12A6; glyphHexadecimal = $F12A7;
  glyphGestureTapButton = $F12A8; glyphGestureTapBox = $F12A9; glyphLanCheck = $F12AA;
  glyphKeyboardF1 = $F12AB; glyphKeyboardF2 = $F12AC; glyphKeyboardF3 = $F12AD;
  glyphKeyboardF4 = $F12AE; glyphKeyboardF5 = $F12AF; glyphKeyboardF6 = $F12B0;
  glyphKeyboardF7 = $F12B1; glyphKeyboardF8 = $F12B2; glyphKeyboardF9 = $F12B3;
  glyphKeyboardF10 = $F12B4; glyphKeyboardF11 = $F12B5; glyphKeyboardF12 = $F12B6;
  glyphKeyboardEsc = $F12B7; glyphToslink = $F12B8; glyphCheese = $F12B9;
  glyphStringLights = $F12BA; glyphStringLightsOff = $F12BB; glyphWhistleOutline = $F12BC;
  glyphStairsUp = $F12BD; glyphStairsDown = $F12BE; glyphEscalatorUp = $F12BF;
  glyphEscalatorDown = $F12C0; glyphElevatorUp = $F12C1; glyphElevatorDown = $F12C2;
  glyphLightbulbCflSpiralOff = $F12C3; glyphCommentEditOutline = $F12C4; glyphTooltipEditOutline = $F12C5;
  glyphMonitorEdit = $F12C6; glyphEmailSync = $F12C7; glyphEmailSyncOutline = $F12C8;
  glyphChatAlertOutline = $F12C9; glyphChatProcessingOutline = $F12CA; glyphSnowflakeMelt = $F12CB;
  glyphCloudCheckOutline = $F12CC; glyphLightbulbGroupOff = $F12CD; glyphLightbulbGroupOffOutline = $F12CE;
  glyphLightbulbMultipleOff = $F12CF; glyphLightbulbMultipleOffOutline = $F12D0; glyphChatSleep = $F12D1;
  glyphChatSleepOutline = $F12D2; glyphGarageVariant = $F12D3; glyphGarageOpenVariant = $F12D4;
  glyphGarageAlertVariant = $F12D5; glyphCloudSyncOutline = $F12D6; glyphGlobeLight = $F12D7;
  glyphCellphoneNfcOff = $F12D8; glyphLeafOff = $F12D9; glyphLeafMapleOff = $F12DA;
  glyphMapMarkerLeft = $F12DB; glyphMapMarkerRight = $F12DC; glyphMapMarkerLeftOutline = $F12DD;
  glyphMapMarkerRightOutline = $F12DE; glyphAccountCancel = $F12DF; glyphAccountCancelOutline = $F12E0;
  glyphFileClock = $F12E1; glyphFileClockOutline = $F12E2; glyphFolderTable = $F12E3;
  glyphFolderTableOutline = $F12E4; glyphHydroPower = $F12E5; glyphDoorbell = $F12E6;
  glyphBulma = $F12E7; glyphIobroker = $F12E8; glyphOci = $F12E9;
  glyphLabelPercent = $F12EA; glyphLabelPercentOutline = $F12EB; glyphCheckboxBlankOff = $F12EC;
  glyphCheckboxBlankOffOutline = $F12ED; glyphSquareOff = $F12EE; glyphSquareOffOutline = $F12EF;
  glyphDragHorizontalVariant = $F12F0; glyphDragVerticalVariant = $F12F1; glyphMessageArrowLeft = $F12F2;
  glyphMessageArrowLeftOutline = $F12F3; glyphMessageArrowRight = $F12F4; glyphMessageArrowRightOutline = $F12F5;
  glyphDatabaseMarker = $F12F6; glyphTagMultipleOutline = $F12F7; glyphMapMarkerPlusOutline = $F12F8;
  glyphMapMarkerMinusOutline = $F12F9; glyphMapMarkerRemoveOutline = $F12FA; glyphMapMarkerCheckOutline = $F12FB;
  glyphMapMarkerRadiusOutline = $F12FC; glyphMapMarkerOffOutline = $F12FD; glyphMoleculeCo = $F12FE;
  glyphJumpRope = $F12FF; glyphKettlebell = $F1300; glyphAccountConvertOutline = $F1301;
  glyphBunkBed = $F1302; glyphFleurDeLis = $F1303; glyphSki = $F1304;
  glyphSkiCrossCountry = $F1305; glyphSkiWater = $F1306; glyphSnowboard = $F1307;
  glyphAccountTieVoice = $F1308; glyphAccountTieVoiceOutline = $F1309; glyphAccountTieVoiceOff = $F130A;
  glyphAccountTieVoiceOffOutline = $F130B; glyphBeerOutline = $F130C; glyphGlassPintOutline = $F130D;
  glyphCoffeeToGoOutline = $F130E; glyphCupOutline = $F130F; glyphBottleWineOutline = $F1310;
  glyphEarthArrowRight = $F1311; glyphKeyArrowRight = $F1312; glyphFormatColorMarkerCancel = $F1313;
  glyphMotherHeart = $F1314; glyphCurrencyEurOff = $F1315; glyphSemanticWeb = $F1316;
  glyphKettleAlert = $F1317; glyphKettleAlertOutline = $F1318; glyphKettleSteam = $F1319;
  glyphKettleSteamOutline = $F131A; glyphKettleOff = $F131B; glyphKettleOffOutline = $F131C;
  glyphSimpleIcons = $F131D; glyphBriefcaseCheckOutline = $F131E; glyphClipboardPlusOutline = $F131F;
  glyphDownloadLock = $F1320; glyphDownloadLockOutline = $F1321; glyphHammerScrewdriver = $F1322;
  glyphHammerWrench = $F1323; glyphHydraulicOilLevel = $F1324; glyphHydraulicOilTemperature = $F1325;
  glyphMedalOutline = $F1326; glyphRodent = $F1327; glyphAbjadArabic = $F1328;
  glyphAbjadHebrew = $F1329; glyphAbugidaDevanagari = $F132A; glyphAbugidaThai = $F132B;
  glyphAlphabetAurebesh = $F132C; glyphAlphabetCyrillic = $F132D; glyphAlphabetGreek = $F132E;
  glyphAlphabetLatin = $F132F; glyphAlphabetPiqad = $F1330; glyphIdeogramCjk = $F1331;
  glyphIdeogramCjkVariant = $F1332; glyphSyllabaryHangul = $F1333; glyphSyllabaryHiragana = $F1334;
  glyphSyllabaryKatakana = $F1335; glyphSyllabaryKatakanaHalfwidth = $F1336; glyphAlphabetTengwar = $F1337;
  glyphHeadAlert = $F1338; glyphHeadAlertOutline = $F1339; glyphHeadCheck = $F133A;
  glyphHeadCheckOutline = $F133B; glyphHeadCog = $F133C; glyphHeadCogOutline = $F133D;
  glyphHeadDotsHorizontal = $F133E; glyphHeadDotsHorizontalOutline = $F133F; glyphHeadFlash = $F1340;
  glyphHeadFlashOutline = $F1341; glyphHeadHeart = $F1342; glyphHeadHeartOutline = $F1343;
  glyphHeadLightbulb = $F1344; glyphHeadLightbulbOutline = $F1345; glyphHeadMinus = $F1346;
  glyphHeadMinusOutline = $F1347; glyphHeadPlus = $F1348; glyphHeadPlusOutline = $F1349;
  glyphHeadQuestion = $F134A; glyphHeadQuestionOutline = $F134B; glyphHeadRemove = $F134C;
  glyphHeadRemoveOutline = $F134D; glyphHeadSnowflake = $F134E; glyphHeadSnowflakeOutline = $F134F;
  glyphHeadSync = $F1350; glyphHeadSyncOutline = $F1351; glyphHvac = $F1352;
  glyphPencilRuler = $F1353; glyphPipeWrench = $F1354; glyphWidgetsOutline = $F1355;
  glyphTelevisionAmbientLight = $F1356; glyphPropaneTank = $F1357; glyphPropaneTankOutline = $F1358;
  glyphFolderMusic = $F1359; glyphFolderMusicOutline = $F135A; glyphKlingon = $F135B;
  glyphPaletteSwatchOutline = $F135C; glyphFormTextboxLock = $F135D; glyphHead = $F135E;
  glyphHeadOutline = $F135F; glyphShieldHalf = $F1360; glyphStoreOutline = $F1361;
  glyphGoogleDownasaur = $F1362; glyphBottleSodaClassicOutline = $F1363; glyphSticker = $F1364;
  glyphStickerAlert = $F1365; glyphStickerAlertOutline = $F1366; glyphStickerCheck = $F1367;
  glyphStickerCheckOutline = $F1368; glyphStickerMinus = $F1369; glyphStickerMinusOutline = $F136A;
  glyphStickerOutline = $F136B; glyphStickerPlus = $F136C; glyphStickerPlusOutline = $F136D;
  glyphStickerRemove = $F136E; glyphStickerRemoveOutline = $F136F; glyphAccountCog = $F1370;
  glyphAccountCogOutline = $F1371; glyphAccountDetailsOutline = $F1372; glyphUploadLock = $F1373;
  glyphUploadLockOutline = $F1374; glyphLabelMultiple = $F1375; glyphLabelMultipleOutline = $F1376;
  glyphRefreshCircle = $F1377; glyphSyncCircle = $F1378; glyphBookmarkMusicOutline = $F1379;
  glyphBookmarkRemoveOutline = $F137A; glyphBookmarkCheckOutline = $F137B; glyphTrafficCone = $F137C;
  glyphCupOffOutline = $F137D; glyphAutoDownload = $F137E; glyphShuriken = $F137F;
  glyphChartPpf = $F1380; glyphElevatorPassenger = $F1381; glyphCompassRose = $F1382;
  glyphSpaceStation = $F1383; glyphOrderBoolDescending = $F1384; glyphSortBoolAscending = $F1385;
  glyphSortBoolAscendingVariant = $F1386; glyphSortBoolDescending = $F1387; glyphSortBoolDescendingVariant = $F1388;
  glyphSortNumericAscending = $F1389; glyphSortNumericDescending = $F138A; glyphHumanBabyChangingTable = $F138B;
  glyphHumanMaleChild = $F138C; glyphHumanWheelchair = $F138D; glyphMicrosoftAccess = $F138E;
  glyphMicrosoftExcel = $F138F; glyphMicrosoftPowerpoint = $F1390; glyphMicrosoftSharepoint = $F1391;
  glyphMicrosoftWord = $F1392; glyphNintendoGameBoy = $F1393; glyphCableData = $F1394;
  glyphCircleHalf = $F1395; glyphCircleHalfFull = $F1396; glyphCellphoneCharging = $F1397;
  glyphCloseThick = $F1398; glyphEscalatorBox = $F1399; glyphLockCheck = $F139A;
  glyphLockOpenAlert = $F139B; glyphLockOpenCheck = $F139C; glyphRecycleVariant = $F139D;
  glyphStairsBox = $F139E; glyphHandWater = $F139F; glyphTableRefresh = $F13A0;
  glyphTableSync = $F13A1; glyphSizeXxs = $F13A2; glyphSizeXs = $F13A3;
  glyphSizeS = $F13A4; glyphSizeM = $F13A5; glyphSizeXl = $F13A7;
  glyphSizeXxl = $F13A8; glyphSizeXxxl = $F13A9; glyphTicketConfirmationOutline = $F13AA;
  glyphTimer = $F13AB; glyphTimerOff = $F13AC; glyphBookAccount = $F13AD;
  glyphBookAccountOutline = $F13AE; glyphRocketOutline = $F13AF; glyphHomeSearch = $F13B0;
  glyphHomeSearchOutline = $F13B1; glyphCarArrowLeft = $F13B2; glyphCarArrowRight = $F13B3;
  glyphMonitorEye = $F13B4; glyphLipstick = $F13B5; glyphVirus = $F13B6;
  glyphVirusOutline = $F13B7; glyphTextSearch = $F13B8; glyphTableAccount = $F13B9;
  glyphTableAlert = $F13BA; glyphTableArrowDown = $F13BB; glyphTableArrowLeft = $F13BC;
  glyphTableArrowRight = $F13BD; glyphTableArrowUp = $F13BE; glyphTableCancel = $F13BF;
  glyphTableCheck = $F13C0; glyphTableClock = $F13C1; glyphTableCog = $F13C2;
  glyphTableEyeOff = $F13C3; glyphTableHeart = $F13C4; glyphTableKey = $F13C5;
  glyphTableLock = $F13C6; glyphTableMinus = $F13C7; glyphTableMultiple = $F13C8;
  glyphTableNetwork = $F13C9; glyphTableOff = $F13CA; glyphTableStar = $F13CB;
  glyphCarCog = $F13CC; glyphCarSettings = $F13CD; glyphCogOff = $F13CE;
  glyphCogOffOutline = $F13CF; glyphCreditCardCheck = $F13D0; glyphCreditCardCheckOutline = $F13D1;
  glyphFileTreeOutline = $F13D2; glyphFolderStarMultiple = $F13D3; glyphFolderStarMultipleOutline = $F13D4;
  glyphHomeMinusOutline = $F13D5; glyphHomePlusOutline = $F13D6; glyphHomeRemoveOutline = $F13D7;
  glyphScanHelper = $F13D8; glyphVideo3dOff = $F13D9; glyphShieldBug = $F13DA;
  glyphShieldBugOutline = $F13DB; glyphEyedropperPlus = $F13DC; glyphEyedropperMinus = $F13DD;
  glyphEyedropperRemove = $F13DE; glyphEyedropperOff = $F13DF; glyphBabyBuggy = $F13E0;
  glyphUmbrellaClosedVariant = $F13E1; glyphUmbrellaClosedOutline = $F13E2; glyphEmailOff = $F13E3;
  glyphEmailOffOutline = $F13E4; glyphFoodVariantOff = $F13E5; glyphPlayBoxMultipleOutline = $F13E6;
  glyphBellCancel = $F13E7; glyphBellCancelOutline = $F13E8; glyphBellMinus = $F13E9;
  glyphBellMinusOutline = $F13EA; glyphBellRemove = $F13EB; glyphBellRemoveOutline = $F13EC;
  glyphBeehiveOffOutline = $F13ED; glyphCheeseOff = $F13EE; glyphCornOff = $F13EF;
  glyphEggOff = $F13F0; glyphEggOffOutline = $F13F1; glyphEggOutline = $F13F2;
  glyphFishOff = $F13F3; glyphFlaskEmptyOff = $F13F4; glyphFlaskEmptyOffOutline = $F13F5;
  glyphFlaskOff = $F13F6; glyphFlaskOffOutline = $F13F7; glyphFruitCherriesOff = $F13F8;
  glyphFruitCitrusOff = $F13F9; glyphMushroomOff = $F13FA; glyphMushroomOffOutline = $F13FB;
  glyphSoySauceOff = $F13FC; glyphSeedOff = $F13FD; glyphSeedOffOutline = $F13FE;
  glyphTailwind = $F13FF; glyphFormDropdown = $F1400; glyphFormSelect = $F1401;
  glyphPump = $F1402; glyphEarthPlus = $F1403; glyphEarthMinus = $F1404;
  glyphEarthRemove = $F1405; glyphEarthBoxPlus = $F1406; glyphEarthBoxMinus = $F1407;
  glyphEarthBoxRemove = $F1408; glyphGasStationOff = $F1409; glyphGasStationOffOutline = $F140A;
  glyphLightningBolt = $F140B; glyphLightningBoltOutline = $F140C; glyphSmokingPipe = $F140D;
  glyphAxisArrowInfo = $F140E; glyphChatPlus = $F140F; glyphChatMinus = $F1410;
  glyphChatRemove = $F1411; glyphChatPlusOutline = $F1412; glyphChatMinusOutline = $F1413;
  glyphChatRemoveOutline = $F1414; glyphBucket = $F1415; glyphBucketOutline = $F1416;
  glyphPail = $F1417; glyphImageRemove = $F1418; glyphImageMinus = $F1419;
  glyphPineTreeFire = $F141A; glyphCigarOff = $F141B; glyphCubeOff = $F141C;
  glyphCubeOffOutline = $F141D; glyphDomeLight = $F141E; glyphFoodDrumstick = $F141F;
  glyphFoodDrumstickOutline = $F1420; glyphIncognitoCircle = $F1421; glyphIncognitoCircleOff = $F1422;
  glyphMicrowaveOff = $F1423; glyphPowerPlugOffOutline = $F1424; glyphPowerPlugOutline = $F1425;
  glyphPuzzleCheck = $F1426; glyphPuzzleCheckOutline = $F1427; glyphSmokingPipeOff = $F1428;
  glyphSpoonSugar = $F1429; glyphTableSplitCell = $F142A; glyphTicketPercentOutline = $F142B;
  glyphFuseOff = $F142C; glyphFuseAlert = $F142D; glyphHeartPlus = $F142E;
  glyphHeartMinus = $F142F; glyphHeartRemove = $F1430; glyphHeartPlusOutline = $F1431;
  glyphHeartMinusOutline = $F1432; glyphHeartRemoveOutline = $F1433; glyphHeartOffOutline = $F1434;
  glyphMotionSensorOff = $F1435; glyphPailPlus = $F1436; glyphPailMinus = $F1437;
  glyphPailRemove = $F1438; glyphPailOff = $F1439; glyphPailOutline = $F143A;
  glyphPailPlusOutline = $F143B; glyphPailMinusOutline = $F143C; glyphPailRemoveOutline = $F143D;
  glyphPailOffOutline = $F143E; glyphClockTimeOne = $F143F; glyphClockTimeTwo = $F1440;
  glyphClockTimeThree = $F1441; glyphClockTimeFour = $F1442; glyphClockTimeFive = $F1443;
  glyphClockTimeSix = $F1444; glyphClockTimeSeven = $F1445; glyphClockTimeEight = $F1446;
  glyphClockTimeNine = $F1447; glyphClockTimeTen = $F1448; glyphClockTimeEleven = $F1449;
  glyphClockTimeTwelve = $F144A; glyphClockTimeOneOutline = $F144B; glyphClockTimeTwoOutline = $F144C;
  glyphClockTimeThreeOutline = $F144D; glyphClockTimeFourOutline = $F144E; glyphClockTimeFiveOutline = $F144F;
  glyphClockTimeSixOutline = $F1450; glyphClockTimeSevenOutline = $F1451; glyphClockTimeEightOutline = $F1452;
  glyphClockTimeNineOutline = $F1453; glyphClockTimeTenOutline = $F1454; glyphClockTimeElevenOutline = $F1455;
  glyphClockTimeTwelveOutline = $F1456; glyphPrinterSearch = $F1457; glyphPrinterEye = $F1458;
  glyphMinusCircleOff = $F1459; glyphMinusCircleOffOutline = $F145A; glyphContentSaveCog = $F145B;
  glyphContentSaveCogOutline = $F145C; glyphSetSquare = $F145D; glyphCogRefresh = $F145E;
  glyphCogRefreshOutline = $F145F; glyphCogSync = $F1460; glyphCogSyncOutline = $F1461;
  glyphDownloadBox = $F1462; glyphDownloadBoxOutline = $F1463; glyphDownloadCircle = $F1464;
  glyphDownloadCircleOutline = $F1465; glyphAirHumidifierOff = $F1466; glyphChiliOff = $F1467;
  glyphFoodDrumstickOff = $F1468; glyphFoodDrumstickOffOutline = $F1469; glyphFoodSteak = $F146A;
  glyphFoodSteakOff = $F146B; glyphFanAlert = $F146C; glyphFanChevronDown = $F146D;
  glyphFanChevronUp = $F146E; glyphFanPlus = $F146F; glyphFanMinus = $F1470;
  glyphFanRemove = $F1471; glyphFanSpeed1 = $F1472; glyphFanSpeed2 = $F1473;
  glyphFanSpeed3 = $F1474; glyphRug = $F1475; glyphLingerie = $F1476;
  glyphWizardHat = $F1477; glyphHours24 = $F1478; glyphCosineWave = $F1479;
  glyphSawtoothWave = $F147A; glyphSquareWave = $F147B; glyphTriangleWave = $F147C;
  glyphWaveform = $F147D; glyphFolderMultiplePlus = $F147E; glyphFolderMultiplePlusOutline = $F147F;
  glyphCurrentAc = $F1480; glyphWateringCan = $F1481; glyphWateringCanOutline = $F1482;
  glyphMonitorShare = $F1483; glyphLaserPointer = $F1484; glyphViewArrayOutline = $F1485;
  glyphViewCarouselOutline = $F1486; glyphViewColumnOutline = $F1487; glyphViewComfyOutline = $F1488;
  glyphViewDashboardVariantOutline = $F1489; glyphViewDayOutline = $F148A; glyphViewListOutline = $F148B;
  glyphViewModuleOutline = $F148C; glyphViewParallelOutline = $F148D; glyphViewQuiltOutline = $F148E;
  glyphViewSequentialOutline = $F148F; glyphViewStreamOutline = $F1490; glyphViewWeekOutline = $F1491;
  glyphCompareHorizontal = $F1492; glyphCompareVertical = $F1493; glyphBriefcaseVariant = $F1494;
  glyphBriefcaseVariantOutline = $F1495; glyphRelationManyToMany = $F1496; glyphRelationManyToOne = $F1497;
  glyphRelationManyToOneOrMany = $F1498; glyphRelationManyToOnlyOne = $F1499; glyphRelationManyToZeroOrMany = $F149A;
  glyphRelationManyToZeroOrOne = $F149B; glyphRelationOneOrManyToMany = $F149C; glyphRelationOneOrManyToOne = $F149D;
  glyphRelationOneOrManyToOneOrMany = $F149E; glyphRelationOneOrManyToOnlyOne = $F149F; glyphRelationOneOrManyToZeroOrMany = $F14A0;
  glyphRelationOneOrManyToZeroOrOne = $F14A1; glyphRelationOneToMany = $F14A2; glyphRelationOneToOne = $F14A3;
  glyphRelationOneToOneOrMany = $F14A4; glyphRelationOneToOnlyOne = $F14A5; glyphRelationOneToZeroOrMany = $F14A6;
  glyphRelationOneToZeroOrOne = $F14A7; glyphRelationOnlyOneToMany = $F14A8; glyphRelationOnlyOneToOne = $F14A9;
  glyphRelationOnlyOneToOneOrMany = $F14AA; glyphRelationOnlyOneToOnlyOne = $F14AB; glyphRelationOnlyOneToZeroOrMany = $F14AC;
  glyphRelationOnlyOneToZeroOrOne = $F14AD; glyphRelationZeroOrManyToMany = $F14AE; glyphRelationZeroOrManyToOne = $F14AF;
  glyphRelationZeroOrManyToOneOrMany = $F14B0; glyphRelationZeroOrManyToOnlyOne = $F14B1; glyphRelationZeroOrManyToZeroOrMany = $F14B2;
  glyphRelationZeroOrManyToZeroOrOne = $F14B3; glyphRelationZeroOrOneToMany = $F14B4; glyphRelationZeroOrOneToOne = $F14B5;
  glyphRelationZeroOrOneToOneOrMany = $F14B6; glyphRelationZeroOrOneToOnlyOne = $F14B7; glyphRelationZeroOrOneToZeroOrMany = $F14B8;
  glyphRelationZeroOrOneToZeroOrOne = $F14B9; glyphAlertPlus = $F14BA; glyphAlertMinus = $F14BB;
  glyphAlertRemove = $F14BC; glyphAlertPlusOutline = $F14BD; glyphAlertMinusOutline = $F14BE;
  glyphAlertRemoveOutline = $F14BF; glyphCarabiner = $F14C0; glyphFencing = $F14C1;
  glyphSkateboard = $F14C2; glyphPolo = $F14C3; glyphTractorVariant = $F14C4;
  glyphRadiologyBox = $F14C5; glyphRadiologyBoxOutline = $F14C6; glyphSkullScan = $F14C7;
  glyphSkullScanOutline = $F14C8; glyphPlusMinusVariant = $F14C9; glyphSourceBranchPlus = $F14CA;
  glyphSourceBranchMinus = $F14CB; glyphSourceBranchRemove = $F14CC; glyphSourceBranchRefresh = $F14CD;
  glyphSourceBranchSync = $F14CE; glyphSourceBranchCheck = $F14CF; glyphPuzzlePlus = $F14D0;
  glyphPuzzleMinus = $F14D1; glyphPuzzleRemove = $F14D2; glyphPuzzleEdit = $F14D3;
  glyphPuzzleHeart = $F14D4; glyphPuzzleStar = $F14D5; glyphPuzzlePlusOutline = $F14D6;
  glyphPuzzleMinusOutline = $F14D7; glyphPuzzleRemoveOutline = $F14D8; glyphPuzzleEditOutline = $F14D9;
  glyphPuzzleHeartOutline = $F14DA; glyphPuzzleStarOutline = $F14DB; glyphRhombusMediumOutline = $F14DC;
  glyphRhombusSplitOutline = $F14DD; glyphRocketLaunch = $F14DE; glyphRocketLaunchOutline = $F14DF;
  glyphSetMerge = $F14E0; glyphSetSplit = $F14E1; glyphBeekeeper = $F14E2;
  glyphSnowflakeOff = $F14E3; glyphWeatherSunnyOff = $F14E4; glyphClipboardEdit = $F14E5;
  glyphClipboardEditOutline = $F14E6; glyphNotebookEdit = $F14E7; glyphHumanEdit = $F14E8;
  glyphNotebookEditOutline = $F14E9; glyphCashLock = $F14EA; glyphCashLockOpen = $F14EB;
  glyphAccountSupervisorCircleOutline = $F14EC; glyphCarOutline = $F14ED; glyphCashCheck = $F14EE;
  glyphFilterOff = $F14EF; glyphFilterOffOutline = $F14F0; glyphSpiritLevel = $F14F1;
  glyphWheelBarrow = $F14F2; glyphBookCheck = $F14F3; glyphBookCheckOutline = $F14F4;
  glyphNotebookCheck = $F14F5; glyphNotebookCheckOutline = $F14F6; glyphBookOpenVariant = $F14F7;
  glyphSignPole = $F14F8; glyphShore = $F14F9; glyphShapeSquareRoundedPlus = $F14FA;
  glyphSquareRounded = $F14FB; glyphSquareRoundedOutline = $F14FC; glyphArchiveAlert = $F14FD;
  glyphArchiveAlertOutline = $F14FE; glyphPowerSocketIt = $F14FF; glyphSquareCircle = $F1500;
  glyphSymbol = $F1501; glyphWaterAlert = $F1502; glyphWaterAlertOutline = $F1503;
  glyphWaterCheck = $F1504; glyphWaterCheckOutline = $F1505; glyphWaterMinus = $F1506;
  glyphWaterMinusOutline = $F1507; glyphWaterOffOutline = $F1508; glyphWaterPercentAlert = $F1509;
  glyphWaterPlus = $F150A; glyphWaterPlusOutline = $F150B; glyphWaterRemove = $F150C;
  glyphWaterRemoveOutline = $F150D; glyphSnake = $F150E; glyphFormatTextVariantOutline = $F150F;
  glyphGrass = $F1510; glyphAccessPointOff = $F1511; glyphCurrencyMnt = $F1512;
  glyphDockTop = $F1513; glyphShareVariantOutline = $F1514; glyphTransitSkip = $F1515;
  glyphYurt = $F1516; glyphFileDocumentMultiple = $F1517; glyphFileDocumentMultipleOutline = $F1518;
  glyphEvPlugCcs1 = $F1519; glyphEvPlugCcs2 = $F151A; glyphEvPlugChademo = $F151B;
  glyphEvPlugTesla = $F151C; glyphEvPlugType1 = $F151D; glyphEvPlugType2 = $F151E;
  glyphOfficeBuildingOutline = $F151F; glyphOfficeBuildingMarker = $F1520; glyphOfficeBuildingMarkerOutline = $F1521;
  glyphProgressQuestion = $F1522; glyphBasketMinus = $F1523; glyphBasketMinusOutline = $F1524;
  glyphBasketOff = $F1525; glyphBasketOffOutline = $F1526; glyphBasketPlus = $F1527;
  glyphBasketPlusOutline = $F1528; glyphBasketRemove = $F1529; glyphBasketRemoveOutline = $F152A;
  glyphAccountReactivate = $F152B; glyphAccountReactivateOutline = $F152C; glyphCarLiftedPickup = $F152D;
  glyphVideoHighDefinition = $F152E; glyphPhoneRemove = $F152F; glyphPhoneRemoveOutline = $F1530;
  glyphThermometerOff = $F1531; glyphTimelineCheck = $F1532; glyphTimelineCheckOutline = $F1533;
  glyphTimelineMinus = $F1534; glyphTimelineMinusOutline = $F1535; glyphTimelineRemove = $F1536;
  glyphTimelineRemoveOutline = $F1537; glyphAccessPointCheck = $F1538; glyphAccessPointMinus = $F1539;
  glyphAccessPointPlus = $F153A; glyphAccessPointRemove = $F153B; glyphDataMatrix = $F153C;
  glyphDataMatrixEdit = $F153D; glyphDataMatrixMinus = $F153E; glyphDataMatrixPlus = $F153F;
  glyphDataMatrixRemove = $F1540; glyphDataMatrixScan = $F1541; glyphTuneVariant = $F1542;
  glyphTuneVerticalVariant = $F1543; glyphRake = $F1544; glyphShimmer = $F1545;
  glyphTransitConnectionHorizontal = $F1546; glyphSortCalendarAscending = $F1547; glyphSortCalendarDescending = $F1548;
  glyphSortClockAscending = $F1549; glyphSortClockAscendingOutline = $F154A; glyphSortClockDescending = $F154B;
  glyphSortClockDescendingOutline = $F154C; glyphChartBox = $F154D; glyphChartBoxOutline = $F154E;
  glyphChartBoxPlusOutline = $F154F; glyphMouseMoveDown = $F1550; glyphMouseMoveUp = $F1551;
  glyphMouseMoveVertical = $F1552; glyphPitchfork = $F1553; glyphVanishQuarter = $F1554;
  glyphApplicationSettingsOutline = $F1555; glyphDeleteClock = $F1556; glyphDeleteClockOutline = $F1557;
  glyphKangaroo = $F1558; glyphPhoneDial = $F1559; glyphPhoneDialOutline = $F155A;
  glyphStarOffOutline = $F155B; glyphTooltipCheck = $F155C; glyphTooltipCheckOutline = $F155D;
  glyphTooltipMinus = $F155E; glyphTooltipMinusOutline = $F155F; glyphTooltipRemove = $F1560;
  glyphTooltipRemoveOutline = $F1561; glyphPretzel = $F1562; glyphStarPlus = $F1563;
  glyphStarMinus = $F1564; glyphStarRemove = $F1565; glyphStarCheck = $F1566;
  glyphStarPlusOutline = $F1567; glyphStarMinusOutline = $F1568; glyphStarRemoveOutline = $F1569;
  glyphStarCheckOutline = $F156A; glyphEiffelTower = $F156B; glyphSubmarine = $F156C;
  glyphSofaOutline = $F156D; glyphSofaSingle = $F156E; glyphSofaSingleOutline = $F156F;
  glyphTextAccount = $F1570; glyphHumanQueue = $F1571; glyphFoodHalal = $F1572;
  glyphFoodKosher = $F1573; glyphKeyChain = $F1574; glyphKeyChainVariant = $F1575;
  glyphLamps = $F1576; glyphApplicationCogOutline = $F1577; glyphDancePole = $F1578;
  glyphSocialDistance2Meters = $F1579; glyphSocialDistance6Feet = $F157A; glyphCalendarCursor = $F157B;
  glyphEmoticonSick = $F157C; glyphEmoticonSickOutline = $F157D; glyphHandHeartOutline = $F157E;
  glyphHandWash = $F157F; glyphHandWashOutline = $F1580; glyphHumanCane = $F1581;
  glyphLotion = $F1582; glyphLotionOutline = $F1583; glyphLotionPlus = $F1584;
  glyphLotionPlusOutline = $F1585; glyphFaceMask = $F1586; glyphFaceMaskOutline = $F1587;
  glyphReiterate = $F1588; glyphButterfly = $F1589; glyphButterflyOutline = $F158A;
  glyphBagSuitcase = $F158B; glyphBagSuitcaseOutline = $F158C; glyphBagSuitcaseOff = $F158D;
  glyphBagSuitcaseOffOutline = $F158E; glyphMotionPlay = $F158F; glyphMotionPause = $F1590;
  glyphMotionPlayOutline = $F1591; glyphMotionPauseOutline = $F1592; glyphArrowTopLeftThinCircleOutline = $F1593;
  glyphArrowTopRightThinCircleOutline = $F1594; glyphArrowBottomRightThinCircleOutline = $F1595; glyphArrowBottomLeftThinCircleOutline = $F1596;
  glyphArrowUpThinCircleOutline = $F1597; glyphArrowRightThinCircleOutline = $F1598; glyphArrowDownThinCircleOutline = $F1599;
  glyphArrowLeftThinCircleOutline = $F159A; glyphHumanCapacityDecrease = $F159B; glyphHumanCapacityIncrease = $F159C;
  glyphHumanGreetingProximity = $F159D; glyphHvacOff = $F159E; glyphInboxRemove = $F159F;
  glyphInboxRemoveOutline = $F15A0; glyphHandshakeOutline = $F15A1; glyphLadder = $F15A2;
  glyphRouterWirelessOff = $F15A3; glyphSeesaw = $F15A4; glyphSlide = $F15A5;
  glyphCalculatorVariantOutline = $F15A6; glyphShieldAccountVariant = $F15A7; glyphShieldAccountVariantOutline = $F15A8;
  glyphMessageFlash = $F15A9; glyphMessageFlashOutline = $F15AA; glyphListStatus = $F15AB;
  glyphMessageBookmark = $F15AC; glyphMessageBookmarkOutline = $F15AD; glyphCommentBookmark = $F15AE;
  glyphCommentBookmarkOutline = $F15AF; glyphCommentFlash = $F15B0; glyphCommentFlashOutline = $F15B1;
  glyphMotion = $F15B2; glyphMotionOutline = $F15B3; glyphBicycleElectric = $F15B4;
  glyphCarElectricOutline = $F15B5; glyphChartTimelineVariantShimmer = $F15B6; glyphMopedElectric = $F15B7;
  glyphMopedElectricOutline = $F15B8; glyphMopedOutline = $F15B9; glyphMotorbikeElectric = $F15BA;
  glyphRickshaw = $F15BB; glyphRickshawElectric = $F15BC; glyphScooter = $F15BD;
  glyphScooterElectric = $F15BE; glyphHorse = $F15BF; glyphHorseHuman = $F15C0;
  glyphHorseVariant = $F15C1; glyphUnicorn = $F15C2; glyphUnicornVariant = $F15C3;
  glyphAlarmPanel = $F15C4; glyphAlarmPanelOutline = $F15C5; glyphBird = $F15C6;
  glyphShoeCleat = $F15C7; glyphShoeSneaker = $F15C8; glyphHumanFemaleDance = $F15C9;
  glyphShoeBallet = $F15CA; glyphNumericPositive1 = $F15CB; glyphFaceManShimmer = $F15CC;
  glyphFaceManShimmerOutline = $F15CD; glyphFaceWomanShimmer = $F15CE; glyphFaceWomanShimmerOutline = $F15CF;
  glyphHomeAlertOutline = $F15D0; glyphLockAlertOutline = $F15D1; glyphLockOpenAlertOutline = $F15D2;
  glyphSimAlertOutline = $F15D3; glyphSimOffOutline = $F15D4; glyphSimOutline = $F15D5;
  glyphBookOpenPageVariantOutline = $F15D6; glyphFireAlert = $F15D7; glyphRayStartVertexEnd = $F15D8;
  glyphCameraFlip = $F15D9; glyphCameraFlipOutline = $F15DA; glyphOrbitVariant = $F15DB;
  glyphCircleBox = $F15DC; glyphCircleBoxOutline = $F15DD; glyphMustache = $F15DE;
  glyphCommentMinus = $F15DF; glyphCommentMinusOutline = $F15E0; glyphCommentOff = $F15E1;
  glyphCommentOffOutline = $F15E2; glyphEyeRemove = $F15E3; glyphEyeRemoveOutline = $F15E4;
  glyphUnicycle = $F15E5; glyphGlassCocktailOff = $F15E6; glyphGlassMugOff = $F15E7;
  glyphGlassMugVariantOff = $F15E8; glyphBicyclePennyFarthing = $F15E9; glyphCartCheck = $F15EA;
  glyphCartVariant = $F15EB; glyphBaseballDiamond = $F15EC; glyphBaseballDiamondOutline = $F15ED;
  glyphFridgeIndustrial = $F15EE; glyphFridgeIndustrialAlert = $F15EF; glyphFridgeIndustrialAlertOutline = $F15F0;
  glyphFridgeIndustrialOff = $F15F1; glyphFridgeIndustrialOffOutline = $F15F2; glyphFridgeIndustrialOutline = $F15F3;
  glyphFridgeVariant = $F15F4; glyphFridgeVariantAlert = $F15F5; glyphFridgeVariantAlertOutline = $F15F6;
  glyphFridgeVariantOff = $F15F7; glyphFridgeVariantOffOutline = $F15F8; glyphFridgeVariantOutline = $F15F9;
  glyphWindsock = $F15FA; glyphDanceBallroom = $F15FB; glyphDotsGrid = $F15FC;
  glyphDotsSquare = $F15FD; glyphDotsTriangle = $F15FE; glyphDotsHexagon = $F15FF;
  glyphCardMinus = $F1600; glyphCardMinusOutline = $F1601; glyphCardOff = $F1602;
  glyphCardOffOutline = $F1603; glyphCardRemove = $F1604; glyphCardRemoveOutline = $F1605;
  glyphTorch = $F1606; glyphNavigationOutline = $F1607; glyphMapMarkerStar = $F1608;
  glyphMapMarkerStarOutline = $F1609; glyphManjaro = $F160A; glyphFastForward60 = $F160B;
  glyphRewind60 = $F160C; glyphImageText = $F160D; glyphFamilyTree = $F160E;
  glyphCarEmergency = $F160F; glyphNotebookMinus = $F1610; glyphNotebookMinusOutline = $F1611;
  glyphNotebookPlus = $F1612; glyphNotebookPlusOutline = $F1613; glyphNotebookRemove = $F1614;
  glyphNotebookRemoveOutline = $F1615; glyphConnection = $F1616; glyphLanguageRust = $F1617;
  glyphClipboardMinus = $F1618; glyphClipboardMinusOutline = $F1619; glyphClipboardOff = $F161A;
  glyphClipboardOffOutline = $F161B; glyphClipboardRemove = $F161C; glyphClipboardRemoveOutline = $F161D;
  glyphClipboardSearch = $F161E; glyphClipboardSearchOutline = $F161F; glyphClipboardTextOff = $F1620;
  glyphClipboardTextOffOutline = $F1621; glyphClipboardTextSearch = $F1622; glyphClipboardTextSearchOutline = $F1623;
  glyphDatabaseAlertOutline = $F1624; glyphDatabaseArrowDownOutline = $F1625; glyphDatabaseArrowLeftOutline = $F1626;
  glyphDatabaseArrowRightOutline = $F1627; glyphDatabaseArrowUpOutline = $F1628; glyphDatabaseCheckOutline = $F1629;
  glyphDatabaseClockOutline = $F162A; glyphDatabaseEditOutline = $F162B; glyphDatabaseExportOutline = $F162C;
  glyphDatabaseImportOutline = $F162D; glyphDatabaseLockOutline = $F162E; glyphDatabaseMarkerOutline = $F162F;
  glyphDatabaseMinusOutline = $F1630; glyphDatabaseOffOutline = $F1631; glyphDatabaseOutline = $F1632;
  glyphDatabasePlusOutline = $F1633; glyphDatabaseRefreshOutline = $F1634; glyphDatabaseRemoveOutline = $F1635;
  glyphDatabaseSearchOutline = $F1636; glyphDatabaseSettingsOutline = $F1637; glyphDatabaseSyncOutline = $F1638;
  glyphMinusThick = $F1639; glyphDatabaseAlert = $F163A; glyphDatabaseArrowDown = $F163B;
  glyphDatabaseArrowLeft = $F163C; glyphDatabaseArrowRight = $F163D; glyphDatabaseArrowUp = $F163E;
  glyphDatabaseClock = $F163F; glyphDatabaseOff = $F1640; glyphCalendarLock = $F1641;
  glyphCalendarLockOutline = $F1642; glyphContentSaveOff = $F1643; glyphContentSaveOffOutline = $F1644;
  glyphCreditCardRefresh = $F1645; glyphCreditCardRefreshOutline = $F1646; glyphCreditCardSearch = $F1647;
  glyphCreditCardSearchOutline = $F1648; glyphCreditCardSync = $F1649; glyphCreditCardSyncOutline = $F164A;
  glyphDatabaseCog = $F164B; glyphDatabaseCogOutline = $F164C; glyphMessageOff = $F164D;
  glyphMessageOffOutline = $F164E; glyphNoteMinus = $F164F; glyphNoteMinusOutline = $F1650;
  glyphNoteRemove = $F1651; glyphNoteRemoveOutline = $F1652; glyphNoteSearch = $F1653;
  glyphNoteSearchOutline = $F1654; glyphBankCheck = $F1655; glyphBankOff = $F1656;
  glyphBankOffOutline = $F1657; glyphBriefcaseOff = $F1658; glyphBriefcaseOffOutline = $F1659;
  glyphBriefcaseVariantOff = $F165A; glyphBriefcaseVariantOffOutline = $F165B; glyphGhostOffOutline = $F165C;
  glyphGhostOutline = $F165D; glyphStoreMinus = $F165E; glyphStorePlus = $F165F;
  glyphStoreRemove = $F1660; glyphEmailRemove = $F1661; glyphEmailRemoveOutline = $F1662;
  glyphHeartCog = $F1663; glyphHeartCogOutline = $F1664; glyphHeartSettings = $F1665;
  glyphHeartSettingsOutline = $F1666; glyphPentagram = $F1667; glyphStarCog = $F1668;
  glyphStarCogOutline = $F1669; glyphStarSettings = $F166A; glyphStarSettingsOutline = $F166B;
  glyphCalendarEnd = $F166C; glyphCalendarStart = $F166D; glyphCannabisOff = $F166E;
  glyphMower = $F166F; glyphMowerBag = $F1670; glyphLockOff = $F1671;
  glyphLockOffOutline = $F1672; glyphSharkFin = $F1673; glyphSharkFinOutline = $F1674;
  glyphPawOutline = $F1675; glyphPawOffOutline = $F1676; glyphSnail = $F1677;
  glyphPigVariantOutline = $F1678; glyphPiggyBankOutline = $F1679; glyphRobotOutline = $F167A;
  glyphRobotOffOutline = $F167B; glyphBookAlert = $F167C; glyphBookAlertOutline = $F167D;
  glyphBookArrowDown = $F167E; glyphBookArrowDownOutline = $F167F; glyphBookArrowLeft = $F1680;
  glyphBookArrowLeftOutline = $F1681; glyphBookArrowRight = $F1682; glyphBookArrowRightOutline = $F1683;
  glyphBookArrowUp = $F1684; glyphBookArrowUpOutline = $F1685; glyphBookCancel = $F1686;
  glyphBookCancelOutline = $F1687; glyphBookClock = $F1688; glyphBookClockOutline = $F1689;
  glyphBookCog = $F168A; glyphBookCogOutline = $F168B; glyphBookEdit = $F168C;
  glyphBookEditOutline = $F168D; glyphBookLockOpenOutline = $F168E; glyphBookLockOutline = $F168F;
  glyphBookMarker = $F1690; glyphBookMarkerOutline = $F1691; glyphBookMinusOutline = $F1692;
  glyphBookMusicOutline = $F1693; glyphBookOff = $F1694; glyphBookOffOutline = $F1695;
  glyphBookPlusOutline = $F1696; glyphBookRefresh = $F1697; glyphBookRefreshOutline = $F1698;
  glyphBookRemoveOutline = $F1699; glyphBookSettings = $F169A; glyphBookSettingsOutline = $F169B;
  glyphBookSync = $F169C; glyphRobotAngry = $F169D; glyphRobotAngryOutline = $F169E;
  glyphRobotConfused = $F169F; glyphRobotConfusedOutline = $F16A0; glyphRobotDead = $F16A1;
  glyphRobotDeadOutline = $F16A2; glyphRobotExcited = $F16A3; glyphRobotExcitedOutline = $F16A4;
  glyphRobotLove = $F16A5; glyphRobotLoveOutline = $F16A6; glyphRobotOff = $F16A7;
  glyphLockCheckOutline = $F16A8; glyphLockMinus = $F16A9; glyphLockMinusOutline = $F16AA;
  glyphLockOpenCheckOutline = $F16AB; glyphLockOpenMinus = $F16AC; glyphLockOpenMinusOutline = $F16AD;
  glyphLockOpenPlus = $F16AE; glyphLockOpenPlusOutline = $F16AF; glyphLockOpenRemove = $F16B0;
  glyphLockOpenRemoveOutline = $F16B1; glyphLockPlusOutline = $F16B2; glyphLockRemove = $F16B3;
  glyphLockRemoveOutline = $F16B4; glyphWifiAlert = $F16B5; glyphWifiArrowDown = $F16B6;
  glyphWifiArrowLeft = $F16B7; glyphWifiArrowLeftRight = $F16B8; glyphWifiArrowRight = $F16B9;
  glyphWifiArrowUp = $F16BA; glyphWifiArrowUpDown = $F16BB; glyphWifiCancel = $F16BC;
  glyphWifiCheck = $F16BD; glyphWifiCog = $F16BE; glyphWifiLock = $F16BF;
  glyphWifiLockOpen = $F16C0; glyphWifiMarker = $F16C1; glyphWifiMinus = $F16C2;
  glyphWifiPlus = $F16C3; glyphWifiRefresh = $F16C4; glyphWifiRemove = $F16C5;
  glyphWifiSettings = $F16C6; glyphWifiSync = $F16C7; glyphBookSyncOutline = $F16C8;
  glyphBookEducation = $F16C9; glyphBookEducationOutline = $F16CA; glyphWifiStrength1LockOpen = $F16CB;
  glyphWifiStrength2LockOpen = $F16CC; glyphWifiStrength3LockOpen = $F16CD; glyphWifiStrength4LockOpen = $F16CE;
  glyphWifiStrengthLockOpenOutline = $F16CF; glyphCookieAlert = $F16D0; glyphCookieAlertOutline = $F16D1;
  glyphCookieCheck = $F16D2; glyphCookieCheckOutline = $F16D3; glyphCookieCog = $F16D4;
  glyphCookieCogOutline = $F16D5; glyphCookiePlus = $F16D6; glyphCookiePlusOutline = $F16D7;
  glyphCookieRemove = $F16D8; glyphCookieRemoveOutline = $F16D9; glyphCookieMinus = $F16DA;
  glyphCookieMinusOutline = $F16DB; glyphCookieSettings = $F16DC; glyphCookieSettingsOutline = $F16DD;
  glyphCookieOutline = $F16DE; glyphTapeDrive = $F16DF; glyphAbacus = $F16E0;
  glyphCalendarClockOutline = $F16E1; glyphClipboardClock = $F16E2; glyphClipboardClockOutline = $F16E3;
  glyphCookieClock = $F16E4; glyphCookieClockOutline = $F16E5; glyphCookieEdit = $F16E6;
  glyphCookieEditOutline = $F16E7; glyphCookieLock = $F16E8; glyphCookieLockOutline = $F16E9;
  glyphCookieOff = $F16EA; glyphCookieOffOutline = $F16EB; glyphCookieRefresh = $F16EC;
  glyphCookieRefreshOutline = $F16ED; glyphDogSideOff = $F16EE; glyphGiftOff = $F16EF;
  glyphGiftOffOutline = $F16F0; glyphGiftOpen = $F16F1; glyphGiftOpenOutline = $F16F2;
  glyphMovieCheck = $F16F3; glyphMovieCheckOutline = $F16F4; glyphMovieCog = $F16F5;
  glyphMovieCogOutline = $F16F6; glyphMovieMinus = $F16F7; glyphMovieMinusOutline = $F16F8;
  glyphMovieOff = $F16F9; glyphMovieOffOutline = $F16FA; glyphMovieOpenCheck = $F16FB;
  glyphMovieOpenCheckOutline = $F16FC; glyphMovieOpenCog = $F16FD; glyphMovieOpenCogOutline = $F16FE;
  glyphMovieOpenEdit = $F16FF; glyphMovieOpenEditOutline = $F1700; glyphMovieOpenMinus = $F1701;
  glyphMovieOpenMinusOutline = $F1702; glyphMovieOpenOff = $F1703; glyphMovieOpenOffOutline = $F1704;
  glyphMovieOpenPlay = $F1705; glyphMovieOpenPlayOutline = $F1706; glyphMovieOpenPlus = $F1707;
  glyphMovieOpenPlusOutline = $F1708; glyphMovieOpenRemove = $F1709; glyphMovieOpenRemoveOutline = $F170A;
  glyphMovieOpenSettings = $F170B; glyphMovieOpenSettingsOutline = $F170C; glyphMovieOpenStar = $F170D;
  glyphMovieOpenStarOutline = $F170E; glyphMoviePlay = $F170F; glyphMoviePlayOutline = $F1710;
  glyphMoviePlus = $F1711; glyphMoviePlusOutline = $F1712; glyphMovieRemove = $F1713;
  glyphMovieRemoveOutline = $F1714; glyphMovieSettings = $F1715; glyphMovieSettingsOutline = $F1716;
  glyphMovieStar = $F1717; glyphMovieStarOutline = $F1718; glyphRobotHappy = $F1719;
  glyphRobotHappyOutline = $F171A; glyphTurkey = $F171B; glyphFoodTurkey = $F171C;
  glyphFanAuto = $F171D; glyphAlarmLightOff = $F171E; glyphAlarmLightOffOutline = $F171F;
  glyphBroadcast = $F1720; glyphBroadcastOff = $F1721; glyphFireOff = $F1722;
  glyphFireworkOff = $F1723; glyphProjectorScreenOutline = $F1724; glyphScriptTextKey = $F1725;
  glyphScriptTextKeyOutline = $F1726; glyphScriptTextPlay = $F1727; glyphScriptTextPlayOutline = $F1728;
  glyphSurroundSound21 = $F1729; glyphSurroundSound512 = $F172A; glyphTagArrowDown = $F172B;
  glyphTagArrowDownOutline = $F172C; glyphTagArrowLeft = $F172D; glyphTagArrowLeftOutline = $F172E;
  glyphTagArrowRight = $F172F; glyphTagArrowRightOutline = $F1730; glyphTagArrowUp = $F1731;
  glyphTagArrowUpOutline = $F1732; glyphTrainCarPassenger = $F1733; glyphTrainCarPassengerDoor = $F1734;
  glyphTrainCarPassengerDoorOpen = $F1735; glyphTrainCarPassengerVariant = $F1736; glyphWebcamOff = $F1737;
  glyphChatQuestion = $F1738; glyphChatQuestionOutline = $F1739; glyphMessageQuestion = $F173A;
  glyphMessageQuestionOutline = $F173B; glyphKettlePourOver = $F173C; glyphMessageReplyOutline = $F173D;
  glyphMessageReplyTextOutline = $F173E; glyphKoala = $F173F; glyphCheckDecagramOutline = $F1740;
  glyphStarShooting = $F1741; glyphStarShootingOutline = $F1742; glyphTablePicnic = $F1743;
  glyphKitesurfing = $F1744; glyphParagliding = $F1745; glyphSurfing = $F1746;
  glyphFloorLampTorchiere = $F1747; glyphMortarPestle = $F1748; glyphCastAudioVariant = $F1749;
  glyphGradientHorizontal = $F174A; glyphArchiveCancel = $F174B; glyphArchiveCancelOutline = $F174C;
  glyphArchiveCheck = $F174D; glyphArchiveCheckOutline = $F174E; glyphArchiveClock = $F174F;
  glyphArchiveClockOutline = $F1750; glyphArchiveCog = $F1751; glyphArchiveCogOutline = $F1752;
  glyphArchiveEdit = $F1753; glyphArchiveEditOutline = $F1754; glyphArchiveEye = $F1755;
  glyphArchiveEyeOutline = $F1756; glyphArchiveLock = $F1757; glyphArchiveLockOpen = $F1758;
  glyphArchiveLockOpenOutline = $F1759; glyphArchiveLockOutline = $F175A; glyphArchiveMarker = $F175B;
  glyphArchiveMarkerOutline = $F175C; glyphArchiveMinus = $F175D; glyphArchiveMinusOutline = $F175E;
  glyphArchiveMusic = $F175F; glyphArchiveMusicOutline = $F1760; glyphArchiveOff = $F1761;
  glyphArchiveOffOutline = $F1762; glyphArchivePlus = $F1763; glyphArchivePlusOutline = $F1764;
  glyphArchiveRefresh = $F1765; glyphArchiveRefreshOutline = $F1766; glyphArchiveRemove = $F1767;
  glyphArchiveRemoveOutline = $F1768; glyphArchiveSearch = $F1769; glyphArchiveSearchOutline = $F176A;
  glyphArchiveSettings = $F176B; glyphArchiveSettingsOutline = $F176C; glyphArchiveStar = $F176D;
  glyphArchiveStarOutline = $F176E; glyphArchiveSync = $F176F; glyphArchiveSyncOutline = $F1770;
  glyphBrushOff = $F1771; glyphFileImageMarker = $F1772; glyphFileImageMarkerOutline = $F1773;
  glyphFileMarker = $F1774; glyphFileMarkerOutline = $F1775; glyphHamburgerCheck = $F1776;
  glyphHamburgerMinus = $F1777; glyphHamburgerOff = $F1778; glyphHamburgerPlus = $F1779;
  glyphHamburgerRemove = $F177A; glyphImageMarker = $F177B; glyphImageMarkerOutline = $F177C;
  glyphNoteAlert = $F177D; glyphNoteAlertOutline = $F177E; glyphNoteCheck = $F177F;
  glyphNoteCheckOutline = $F1780; glyphNoteEdit = $F1781; glyphNoteEditOutline = $F1782;
  glyphNoteOff = $F1783; glyphNoteOffOutline = $F1784; glyphPrinterOffOutline = $F1785;
  glyphPrinterOutline = $F1786; glyphProgressPencil = $F1787; glyphProgressStar = $F1788;
  glyphSausageOff = $F1789; glyphFolderEye = $F178A; glyphFolderEyeOutline = $F178B;
  glyphInformationOff = $F178C; glyphInformationOffOutline = $F178D; glyphStickerText = $F178E;
  glyphStickerTextOutline = $F178F; glyphWebCancel = $F1790; glyphWebRefresh = $F1791;
  glyphWebSync = $F1792; glyphChandelier = $F1793; glyphHomeSwitch = $F1794;
  glyphHomeSwitchOutline = $F1795; glyphSunSnowflake = $F1796; glyphCeilingFan = $F1797;
  glyphCeilingFanLight = $F1798; glyphSmoke = $F1799; glyphFence = $F179A;
  glyphLightRecessed = $F179B; glyphBatteryLock = $F179C; glyphBatteryLockOpen = $F179D;
  glyphFolderHidden = $F179E; glyphMirrorRectangle = $F179F; glyphMirrorVariant = $F17A0;
  glyphArrowDownLeft = $F17A1; glyphArrowDownLeftBold = $F17A2; glyphArrowDownRight = $F17A3;
  glyphArrowDownRightBold = $F17A4; glyphArrowLeftBottom = $F17A5; glyphArrowLeftBottomBold = $F17A6;
  glyphArrowLeftTop = $F17A7; glyphArrowLeftTopBold = $F17A8; glyphArrowRightBottom = $F17A9;
  glyphArrowRightBottomBold = $F17AA; glyphArrowRightTop = $F17AB; glyphArrowRightTopBold = $F17AC;
  glyphArrowUDownLeft = $F17AD; glyphArrowUDownLeftBold = $F17AE; glyphArrowUDownRight = $F17AF;
  glyphArrowUDownRightBold = $F17B0; glyphArrowULeftBottom = $F17B1; glyphArrowULeftBottomBold = $F17B2;
  glyphArrowULeftTop = $F17B3; glyphArrowULeftTopBold = $F17B4; glyphArrowURightBottom = $F17B5;
  glyphArrowURightBottomBold = $F17B6; glyphArrowURightTop = $F17B7; glyphArrowURightTopBold = $F17B8;
  glyphArrowUUpLeft = $F17B9; glyphArrowUUpLeftBold = $F17BA; glyphArrowUUpRight = $F17BB;
  glyphArrowUUpRightBold = $F17BC; glyphArrowUpLeft = $F17BD; glyphArrowUpLeftBold = $F17BE;
  glyphArrowUpRight = $F17BF; glyphArrowUpRightBold = $F17C0; glyphSelectRemove = $F17C1;
  glyphSelectionEllipseRemove = $F17C2; glyphSelectionRemove = $F17C3; glyphHumanGreeting = $F17C4;
  glyphPh = $F17C5; glyphWaterSync = $F17C6; glyphCeilingLightOutline = $F17C7;
  glyphFloorLampOutline = $F17C8; glyphWallSconceFlatOutline = $F17C9; glyphWallSconceFlatVariantOutline = $F17CA;
  glyphWallSconceOutline = $F17CB; glyphWallSconceRoundOutline = $F17CC; glyphWallSconceRoundVariantOutline = $F17CD;
  glyphFloorLampDualOutline = $F17CE; glyphFloorLampTorchiereVariantOutline = $F17CF; glyphLampOutline = $F17D0;
  glyphLampsOutline = $F17D1; glyphCandelabra = $F17D2; glyphCandelabraFire = $F17D3;
  glyphMenorah = $F17D4; glyphMenorahFire = $F17D5; glyphFloorLampTorchiereOutline = $F17D6;
  glyphCreditCardEdit = $F17D7; glyphCreditCardEditOutline = $F17D8; glyphBriefcaseEye = $F17D9;
  glyphBriefcaseEyeOutline = $F17DA; glyphSoundbar = $F17DB; glyphCrownCircle = $F17DC;
  glyphCrownCircleOutline = $F17DD; glyphBatteryArrowDown = $F17DE; glyphBatteryArrowDownOutline = $F17DF;
  glyphBatteryArrowUp = $F17E0; glyphBatteryArrowUpOutline = $F17E1; glyphBatteryCheck = $F17E2;
  glyphBatteryCheckOutline = $F17E3; glyphBatteryMinus = $F17E4; glyphBatteryMinusOutline = $F17E5;
  glyphBatteryPlus = $F17E6; glyphBatteryPlusOutline = $F17E7; glyphBatteryRemove = $F17E8;
  glyphBatteryRemoveOutline = $F17E9; glyphChiliAlert = $F17EA; glyphChiliAlertOutline = $F17EB;
  glyphChiliHotOutline = $F17EC; glyphChiliMediumOutline = $F17ED; glyphChiliMildOutline = $F17EE;
  glyphChiliOffOutline = $F17EF; glyphCakeVariantOutline = $F17F0; glyphCardMultiple = $F17F1;
  glyphCardMultipleOutline = $F17F2; glyphAccountCowboyHatOutline = $F17F3; glyphLightbulbSpot = $F17F4;
  glyphLightbulbSpotOff = $F17F5; glyphFenceElectric = $F17F6; glyphGateArrowLeft = $F17F7;
  glyphGateAlert = $F17F8; glyphBoomGateUp = $F17F9; glyphBoomGateUpOutline = $F17FA;
  glyphGarageLock = $F17FB; glyphGarageVariantLock = $F17FC; glyphCellphoneCheck = $F17FD;
  glyphSunWireless = $F17FE; glyphSunWirelessOutline = $F17FF; glyphLightbulbAuto = $F1800;
  glyphLightbulbAutoOutline = $F1801; glyphLightbulbVariant = $F1802; glyphLightbulbVariantOutline = $F1803;
  glyphLightbulbFluorescentTube = $F1804; glyphLightbulbFluorescentTubeOutline = $F1805; glyphWaterCircle = $F1806;
  glyphFireCircle = $F1807; glyphSmokeDetectorOutline = $F1808; glyphSmokeDetectorOff = $F1809;
  glyphSmokeDetectorOffOutline = $F180A; glyphSmokeDetectorVariant = $F180B; glyphSmokeDetectorVariantOff = $F180C;
  glyphProjectorScreenOff = $F180D; glyphProjectorScreenOffOutline = $F180E; glyphProjectorScreenVariant = $F180F;
  glyphProjectorScreenVariantOff = $F1810; glyphProjectorScreenVariantOffOutline = $F1811; glyphProjectorScreenVariantOutline = $F1812;
  glyphBrushVariant = $F1813; glyphCarWrench = $F1814; glyphAccountInjury = $F1815;
  glyphAccountInjuryOutline = $F1816; glyphBalcony = $F1817; glyphBathtub = $F1818;
  glyphBathtubOutline = $F1819; glyphBlenderOutline = $F181A; glyphCoffeeMakerOutline = $F181B;
  glyphCountertop = $F181C; glyphCountertopOutline = $F181D; glyphDoorSliding = $F181E;
  glyphDoorSlidingLock = $F181F; glyphDoorSlidingOpen = $F1820; glyphHandWave = $F1821;
  glyphHandWaveOutline = $F1822; glyphHumanMaleFemaleChild = $F1823; glyphIron = $F1824;
  glyphIronOutline = $F1825; glyphLiquidSpot = $F1826; glyphMosque = $F1827;
  glyphShieldMoon = $F1828; glyphShieldMoonOutline = $F1829; glyphTrafficLightOutline = $F182A;
  glyphHandFrontLeft = $F182B; glyphHandBackLeftOutline = $F182C; glyphHandBackRightOutline = $F182D;
  glyphHandFrontLeftOutline = $F182E; glyphHandFrontRightOutline = $F182F; glyphHandBackLeftOff = $F1830;
  glyphHandBackRightOff = $F1831; glyphHandBackLeftOffOutline = $F1832; glyphHandBackRightOffOutline = $F1833;
  glyphBatterySync = $F1834; glyphBatterySyncOutline = $F1835; glyphFoodTakeoutBox = $F1836;
  glyphFoodTakeoutBoxOutline = $F1837; glyphIronBoard = $F1838; glyphPoliceStation = $F1839;
  glyphCellphoneMarker = $F183A; glyphTooltipCellphone = $F183B; glyphTablePivot = $F183C;
  glyphTunnel = $F183D; glyphTunnelOutline = $F183E; glyphArrowProjectileMultiple = $F183F;
  glyphArrowProjectile = $F1840; glyphBowArrow = $F1841; glyphAxeBattle = $F1842;
  glyphMace = $F1843; glyphMagicStaff = $F1844; glyphSpear = $F1845;
  glyphCurtains = $F1846; glyphCurtainsClosed = $F1847; glyphHumanNonBinary = $F1848;
  glyphWaterfall = $F1849; glyphEggFried = $F184A; glyphFoodHotDog = $F184B;
  glyphInduction = $F184C; glyphPipeValve = $F184D; glyphShippingPallet = $F184E;
  glyphEarbuds = $F184F; glyphEarbudsOff = $F1850; glyphEarbudsOffOutline = $F1851;
  glyphEarbudsOutline = $F1852; glyphCircleOpacity = $F1853; glyphSquareOpacity = $F1854;
  glyphWaterOpacity = $F1855; glyphVectorPolygonVariant = $F1856; glyphVectorSquareClose = $F1857;
  glyphVectorSquareOpen = $F1858; glyphWavesArrowLeft = $F1859; glyphWavesArrowRight = $F185A;
  glyphWavesArrowUp = $F185B; glyphCashFast = $F185C; glyphRadioactiveCircle = $F185D;
  glyphRadioactiveCircleOutline = $F185E; glyphCctvOff = $F185F; glyphFormatListGroup = $F1860;
  glyphClockPlus = $F1861; glyphClockPlusOutline = $F1862; glyphClockMinus = $F1863;
  glyphClockMinusOutline = $F1864; glyphClockRemove = $F1865; glyphClockRemoveOutline = $F1866;
  glyphAccountArrowUp = $F1867; glyphAccountArrowDown = $F1868; glyphAccountArrowDownOutline = $F1869;
  glyphAccountArrowUpOutline = $F186A; glyphAudioInputRca = $F186B; glyphAudioInputStereoMinijack = $F186C;
  glyphAudioInputXlr = $F186D; glyphHorseVariantFast = $F186E; glyphEmailFast = $F186F;
  glyphEmailFastOutline = $F1870; glyphCameraDocument = $F1871; glyphCameraDocumentOff = $F1872;
  glyphGlassFragile = $F1873; glyphMagnifyExpand = $F1874; glyphTownHall = $F1875;
  glyphMonitorSmall = $F1876; glyphDiversify = $F1877; glyphCarWireless = $F1878;
  glyphCarSelect = $F1879; glyphAirplaneAlert = $F187A; glyphAirplaneCheck = $F187B;
  glyphAirplaneClock = $F187C; glyphAirplaneCog = $F187D; glyphAirplaneEdit = $F187E;
  glyphAirplaneMarker = $F187F; glyphAirplaneMinus = $F1880; glyphAirplanePlus = $F1881;
  glyphAirplaneRemove = $F1882; glyphAirplaneSearch = $F1883; glyphAirplaneSettings = $F1884;
  glyphFlowerPollen = $F1885; glyphFlowerPollenOutline = $F1886; glyphHammerSickle = $F1887;
  glyphViewGallery = $F1888; glyphViewGalleryOutline = $F1889; glyphUmbrellaBeach = $F188A;
  glyphUmbrellaBeachOutline = $F188B; glyphCabinAFrame = $F188C; glyphAllInclusiveBox = $F188D;
  glyphAllInclusiveBoxOutline = $F188E; glyphHandCoin = $F188F; glyphHandCoinOutline = $F1890;
  glyphTruckFlatbed = $F1891; glyphLayersEdit = $F1892; glyphMulticast = $F1893;
  glyphHydrogenStation = $F1894; glyphThermometerBluetooth = $F1895; glyphTire = $F1896;
  glyphForest = $F1897; glyphAccountTieHat = $F1898; glyphAccountTieHatOutline = $F1899;
  glyphAccountWrench = $F189A; glyphAccountWrenchOutline = $F189B; glyphBicycleCargo = $F189C;
  glyphCalendarCollapseHorizontal = $F189D; glyphCalendarExpandHorizontal = $F189E; glyphCardsClubOutline = $F189F;
  glyphCardsPlaying = $F18A1; glyphCardsPlayingClub = $F18A2; glyphCardsPlayingClubMultiple = $F18A3;
  glyphCardsPlayingClubMultipleOutline = $F18A4; glyphCardsPlayingClubOutline = $F18A5; glyphCardsPlayingDiamond = $F18A6;
  glyphCardsPlayingDiamondMultiple = $F18A7; glyphCardsPlayingDiamondMultipleOutline = $F18A8; glyphCardsPlayingDiamondOutline = $F18A9;
  glyphCardsPlayingHeart = $F18AA; glyphCardsPlayingHeartMultiple = $F18AB; glyphCardsPlayingHeartMultipleOutline = $F18AC;
  glyphCardsPlayingHeartOutline = $F18AD; glyphCardsPlayingSpade = $F18AE; glyphCardsPlayingSpadeMultiple = $F18AF;
  glyphCardsPlayingSpadeMultipleOutline = $F18B0; glyphCardsPlayingSpadeOutline = $F18B1; glyphCardsSpadeOutline = $F18B2;
  glyphCompareRemove = $F18B3; glyphDolphin = $F18B4; glyphFuelCell = $F18B5;
  glyphHandExtended = $F18B6; glyphHandExtendedOutline = $F18B7; glyphPrinter3dNozzleHeat = $F18B8;
  glyphPrinter3dNozzleHeatOutline = $F18B9; glyphShark = $F18BA; glyphSharkOff = $F18BB;
  glyphShieldCrown = $F18BC; glyphShieldCrownOutline = $F18BD; glyphShieldSword = $F18BE;
  glyphShieldSwordOutline = $F18BF; glyphSickle = $F18C0; glyphStoreAlert = $F18C1;
  glyphStoreAlertOutline = $F18C2; glyphStoreCheck = $F18C3; glyphStoreCheckOutline = $F18C4;
  glyphStoreClock = $F18C5; glyphStoreClockOutline = $F18C6; glyphStoreCog = $F18C7;
  glyphStoreCogOutline = $F18C8; glyphStoreEdit = $F18C9; glyphStoreEditOutline = $F18CA;
  glyphStoreMarker = $F18CB; glyphStoreMarkerOutline = $F18CC; glyphStoreMinusOutline = $F18CD;
  glyphStoreOff = $F18CE; glyphStoreOffOutline = $F18CF; glyphStorePlusOutline = $F18D0;
  glyphStoreRemoveOutline = $F18D1; glyphStoreSearch = $F18D2; glyphStoreSearchOutline = $F18D3;
  glyphStoreSettings = $F18D4; glyphStoreSettingsOutline = $F18D5; glyphSunThermometer = $F18D6;
  glyphSunThermometerOutline = $F18D7; glyphTruckCargoContainer = $F18D8; glyphVectorSquareEdit = $F18D9;
  glyphVectorSquareMinus = $F18DA; glyphVectorSquarePlus = $F18DB; glyphVectorSquareRemove = $F18DC;
  glyphCeilingLightMultiple = $F18DD; glyphCeilingLightMultipleOutline = $F18DE; glyphWiperWashAlert = $F18DF;
  glyphCartHeart = $F18E0; glyphVirusOff = $F18E1; glyphVirusOffOutline = $F18E2;
  glyphMapMarkerAccount = $F18E3; glyphMapMarkerAccountOutline = $F18E4; glyphBasketCheck = $F18E5;
  glyphBasketCheckOutline = $F18E6; glyphCreditCardLock = $F18E7; glyphCreditCardLockOutline = $F18E8;
  glyphFormatUnderlineWavy = $F18E9; glyphContentSaveCheck = $F18EA; glyphContentSaveCheckOutline = $F18EB;
  glyphFilterCheck = $F18EC; glyphFilterCheckOutline = $F18ED; glyphFlagOff = $F18EE;
  glyphFlagOffOutline = $F18EF; glyphNavigationVariantOutline = $F18F1; glyphRefreshAuto = $F18F2;
  glyphTildeOff = $F18F3; glyphFitToScreen = $F18F4; glyphFitToScreenOutline = $F18F5;
  glyphWeatherCloudyClock = $F18F6; glyphSmartCardOff = $F18F7; glyphSmartCardOffOutline = $F18F8;
  glyphClipboardTextClock = $F18F9; glyphClipboardTextClockOutline = $F18FA; glyphTeddyBear = $F18FB;
  glyphCowOff = $F18FC; glyphEyeArrowLeft = $F18FD; glyphEyeArrowLeftOutline = $F18FE;
  glyphEyeArrowRight = $F18FF; glyphEyeArrowRightOutline = $F1900; glyphHomeBattery = $F1901;
  glyphHomeBatteryOutline = $F1902; glyphHomeLightningBolt = $F1903; glyphHomeLightningBoltOutline = $F1904;
  glyphLeafCircle = $F1905; glyphLeafCircleOutline = $F1906; glyphTagSearch = $F1907;
  glyphTagSearchOutline = $F1908; glyphCarBrakeFluidLevel = $F1909; glyphCarBrakeLowPressure = $F190A;
  glyphCarBrakeTemperature = $F190B; glyphCarBrakeWornLinings = $F190C; glyphCarLightAlert = $F190D;
  glyphCarSpeedLimiter = $F190E; glyphCreditCardChip = $F190F; glyphCreditCardChipOutline = $F1910;
  glyphCreditCardFast = $F1911; glyphCreditCardFastOutline = $F1912; glyphIntegratedCircuitChip = $F1913;
  glyphThumbsUpDownOutline = $F1914; glyphFoodOffOutline = $F1915; glyphFoodOutline = $F1916;
  glyphFormatPageSplit = $F1917; glyphChartWaterfall = $F1918; glyphGamepadOutline = $F1919;
  glyphNetworkStrength4Cog = $F191A; glyphAccountSync = $F191B; glyphAccountSyncOutline = $F191C;
  glyphBusElectric = $F191D; glyphLiquor = $F191E; glyphDatabaseEye = $F191F;
  glyphDatabaseEyeOff = $F1920; glyphDatabaseEyeOffOutline = $F1921; glyphDatabaseEyeOutline = $F1922;
  glyphTimerSettings = $F1923; glyphTimerSettingsOutline = $F1924; glyphTimerCog = $F1925;
  glyphTimerCogOutline = $F1926; glyphCheckboxMarkedCirclePlusOutline = $F1927; glyphPanoramaHorizontal = $F1928;
  glyphPanoramaVertical = $F1929; glyphAdvertisements = $F192A; glyphAdvertisementsOff = $F192B;
  glyphTransmissionTowerExport = $F192C; glyphTransmissionTowerImport = $F192D; glyphSmokeDetectorAlert = $F192E;
  glyphSmokeDetectorAlertOutline = $F192F; glyphSmokeDetectorVariantAlert = $F1930; glyphCoffeeMakerCheck = $F1931;
  glyphCoffeeMakerCheckOutline = $F1932; glyphCogPause = $F1933; glyphCogPauseOutline = $F1934;
  glyphCogPlay = $F1935; glyphCogPlayOutline = $F1936; glyphCogStop = $F1937;
  glyphCogStopOutline = $F1938; glyphCopyleft = $F1939; glyphFastForward15 = $F193A;
  glyphFileImageMinus = $F193B; glyphFileImageMinusOutline = $F193C; glyphFileImagePlus = $F193D;
  glyphFileImagePlusOutline = $F193E; glyphFileImageRemove = $F193F; glyphFileImageRemoveOutline = $F1940;
  glyphMessageBadge = $F1941; glyphMessageBadgeOutline = $F1942; glyphNewspaperCheck = $F1943;
  glyphNewspaperRemove = $F1944; glyphPublishOff = $F1945; glyphRewind15 = $F1946;
  glyphViewDashboardEdit = $F1947; glyphViewDashboardEditOutline = $F1948; glyphOfficeBuildingCog = $F1949;
  glyphOfficeBuildingCogOutline = $F194A; glyphHandClap = $F194B; glyphCone = $F194C;
  glyphConeOff = $F194D; glyphCylinder = $F194E; glyphCylinderOff = $F194F;
  glyphOctahedron = $F1950; glyphOctahedronOff = $F1951; glyphPyramid = $F1952;
  glyphPyramidOff = $F1953; glyphSphere = $F1954; glyphSphereOff = $F1955;
  glyphFormatLetterSpacing = $F1956; glyphFrenchFries = $F1957; glyphScent = $F1958;
  glyphScentOff = $F1959; glyphPaletteSwatchVariant = $F195A; glyphEmailSeal = $F195B;
  glyphEmailSealOutline = $F195C; glyphStool = $F195D; glyphStoolOutline = $F195E;
  glyphPanoramaWideAngle = $F195F;

implementation

function UnicodeToStr(C: LongWord): string;
begin
  if C = 0 then
    Result := #0
  else if C < $80 then
    Result := Chr(C)
  else if C < $800 then
    Result := Chr((C shr $6) + $C0) + Chr((C and $3F) + $80)
  else if C < $10000 then
    Result := Chr((C shr $C) + $E0) + Chr(((C shr $6) and
      $3F) + $80) + Chr((C and $3F) + $80)
  else if C < $200000 then
    Result := Chr((C shr $12) + $F0) + Chr(((C shr $C) and
      $3F) + $80) + Chr(((C shr $6) and $3F) + $80) +
      Chr((C and $3F) + $80)
  else
    Result := '';
end;

end.

