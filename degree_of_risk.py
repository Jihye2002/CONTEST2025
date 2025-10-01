from PIL import Image, ImageDraw, ImageFont
import math
import os
import textwrap

# -------------------------
# 저장 폴더 설정
save_dir = r"C:\Users\PC_1M\star_degree"
os.makedirs(save_dir, exist_ok=True)

# 한글 폰트 경로
font_path_bold = r"C:\Users\PC_1M\Documents\Noto_Sans_KR\static\NotoSansKR-Bold.ttf"
font_path_regular = r"C:\Users\PC_1M\Documents\Noto_Sans_KR\static\NotoSansKR-Regular.ttf"

# -------------------------
# 위험도 기본값과 가중치
base_risk = {"유흥업소": 50, "어린이보호구역": 30, "편의점": 40, "카페": 40, "빨래방": 35}
weights = {"유흥업소": 1.5, "어린이보호구역": 0.5, "편의점": 1.0, "카페": 1.0, "빨래방": 0.8}

# -------------------------
# 장소 데이터 (서울 내 임의 선정 7개)
places = [
    {"name": "종로 스타노래방 '별빛노래방'", "type": "유흥업소"},
    {"name": "중구 어린이집 '반짝반짝 어린이집'", "type": "어린이보호구역"},
    {"name": "영등포 편의점 '행복편의점'", "type": "편의점"},
    {"name": "강남 카페 '달콤카페'", "type": "카페"},
    {"name": "도봉 빨래방 '깨끗한 빨래방'", "type": "빨래방"},
    {"name": "마포 노래방 '신나는 노래방'", "type": "유흥업소"},
    {"name": "성북 어린이집 '아이사랑 어린이집'", "type": "어린이보호구역"},
]

# 가중치 적용하여 위험도 계산
for place in places:
    base = base_risk.get(place["type"], 40)
    weight = weights.get(place["type"], 1.0)
    place["risk"] = min(int(base * weight), 100)

# -------------------------
# 별 생성 함수 (그라데이션 + 라운드)
def create_star_gradient(risk=65, base_size=150, upscale=4):
    size = base_size * upscale
    img = Image.new("RGBA", (size, size), "white")
    draw = ImageDraw.Draw(img)
    cx, cy = size//2, size//2

    # 별 좌표 (끝 라운드 처리)
    outer = size // 2 - 5 * upscale
    inner = outer * 0.5
    points = []
    for i in range(10):
        r = outer if i % 2 == 0 else inner
        angle = math.radians(i * 36)
        x = cx + r * math.sin(angle)
        y = cy - r * math.cos(angle)
        points.append((x, y))

    # 마스크 생성
    mask = Image.new("L", (size, size), 0)
    mask_draw = ImageDraw.Draw(mask)
    mask_draw.polygon(points, fill=255)

    # 위험도별 색상 그라데이션
    if risk <= 40:
        start_color = (144, 238, 144)  # 연초록
        end_color = (255, 255, 0)      # 노랑
    elif risk <= 60:
        start_color = (255, 255, 0)    # 노랑
        end_color = (255, 165, 0)      # 주황
    else:
        start_color = (255, 165, 0)    # 주황
        end_color = (255, 69, 0)       # 빨강

    # 채움
    layer = Image.new("RGBA", (size, size), "white")
    layer_draw = ImageDraw.Draw(layer)
    fill_height = int(size * risk / 100)
    for y in range(size - fill_height, size):
        ratio = (y - (size - fill_height)) / fill_height
        r_col = int(start_color[0] + (end_color[0] - start_color[0]) * ratio)
        g_col = int(start_color[1] + (end_color[1] - start_color[1]) * ratio)
        b_col = int(start_color[2] + (end_color[2] - start_color[2]) * ratio)
        layer_draw.line([(0, y), (size, y)], fill=(r_col, g_col, b_col, 255))
    img.paste(layer, (0,0), mask)

    # 테두리 노란색
    draw.line(points + [points[0]], fill=(255, 215, 0, 255), width=10, joint="curve")

    # 백분율 표시
    try:
        font_percent = ImageFont.truetype(font_path_bold, int(30 * upscale))
    except:
        font_percent = ImageFont.load_default()
    percent_text = f"{risk}%"
    bbox = draw.textbbox((0,0), percent_text, font=font_percent)
    tw, th = bbox[2]-bbox[0], bbox[3]-bbox[1]
    draw.text((cx - tw/2, cy - th/2), percent_text, font=font_percent, fill="black")

    img = img.resize((base_size, base_size), Image.LANCZOS)
    return img

# -------------------------
# 별 + 장소 이름
def create_star_with_name_gradient(name, risk=65, base_size=150):
    star_img = create_star_gradient(risk=risk, base_size=base_size)

    # 줄바꿈 처리
    max_line_width = 10
    lines = textwrap.wrap(name, width=max_line_width)
    line_height = 24
    padding = 10
    text_area_height = line_height * len(lines) + padding * 2

    canvas_width = base_size
    canvas_height = base_size + text_area_height
    final_img = Image.new("RGBA", (canvas_width, canvas_height), "white")
    final_img.paste(star_img, (0,0))

    draw = ImageDraw.Draw(final_img)
    try:
        font_name = ImageFont.truetype(font_path_regular, 20)
    except:
        font_name = ImageFont.load_default()

    for i, line in enumerate(lines):
        text_y = base_size + padding + i * line_height
        draw.text((canvas_width // 2, text_y), line, font=font_name, fill="black", anchor="mm")

    return final_img

# -------------------------
# 이미지 생성 및 저장
for place in places:
    img = create_star_with_name_gradient(place["name"], place["risk"])
    filename = os.path.join(save_dir, f"{place['name'][:10].replace(' ', '_')}_grad.png")
    img.save(filename)
    print(f"Saved: {filename} (위험도: {place['risk']}%)")
