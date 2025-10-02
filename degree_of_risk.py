import folium
import pandas as pd
import numpy as np
from geopy.distance import geodesic
from branca.element import Template, MacroElement
import random

# -----------------------
# CSV 경로
# -----------------------
entertain_path = r"C:\Users\PC_1M\Desktop\서울시 중구 유흥주점영업 인허가 정보.csv"
protect_path = r"C:\Users\PC_1M\Desktop\중구_어린이보호구역.csv"

# -----------------------
# CSV 읽기
# -----------------------
df_entertain = pd.read_csv(entertain_path, encoding = 'cp949')
df_protect = pd.read_csv(protect_path, encoding = 'cp949')

# 컬럼명 통일
df_entertain = df_entertain.rename(columns={'좌표정보(Y)': 'latitude','좌표정보(X)': 'longitude','사업장명': 'name'})
df_protect = df_protect.rename(columns={'좌표정보(Y)': 'latitude','좌표정보(X)': 'longitude','시설명': 'name'})

# -----------------------
# 중구 범위 필터링
# -----------------------
lat_min, lat_max = 37.564, 37.571
lon_min, lon_max = 126.980, 126.991
df_entertain = df_entertain[(df_entertain['latitude'].between(lat_min, lat_max)) &
                            (df_entertain['longitude'].between(lon_min, lon_max))]
df_protect = df_protect[(df_protect['latitude'].between(lat_min, lat_max)) &
                        (df_protect['longitude'].between(lon_min, lon_max))]

# -----------------------
# 추가 유흥업소 후보 좌표 (랜덤 선택)
# -----------------------
additional_coords = [
    (37.558188,127.011725),(37.563651,126.983675),(37.561332,127.011816),
    (37.5561,126.9849),(37.56689,126.97495),(37.5557,126.9635),
    (37.567252,127.012543),(37.55951297,127.011605),(37.56157,127.01258),
    (37.56211294,127.0012186)
]

random.shuffle(additional_coords)
selected_coords = additional_coords[:8]
additional_names = [f"영업장_추가{i+1}" for i in range(len(selected_coords))]
df_additional = pd.DataFrame(selected_coords, columns=['latitude','longitude'])
df_additional['name'] = additional_names
df_entertain = pd.concat([df_entertain, df_additional], ignore_index=True)

# -----------------------
# 상가 임의 생성
# -----------------------
num_shops = 15
np.random.seed(42)
shop_lat = np.random.uniform(lat_min, lat_max, num_shops)
shop_lon = np.random.uniform(lon_min, lon_max, num_shops)
shop_names = [f"상가{i+1}" for i in range(num_shops)]
shop_risk = np.random.randint(10, 70, num_shops)
df_shop = pd.DataFrame({'name': shop_names,'latitude': shop_lat,'longitude': shop_lon,'risk': shop_risk})

# -----------------------
# 지도 생성
# -----------------------
center_lat = (lat_min + lat_max)/2
center_lon = (lon_min + lon_max)/2
m = folium.Map(location=[center_lat, center_lon], zoom_start=16)

# -----------------------
# 위험도 계산 함수
# -----------------------
def compute_entertain_risk(lat, lon, df, base_risk=70, radius_m=150):
    count = sum(1 for _, row in df.iterrows() if 0 < geodesic((lat, lon), (row['latitude'], row['longitude'])).meters <= radius_m)
    risk = base_risk + count*5
    return min(risk, 100)

def compute_protect_safety(lat, lon, df_entertain, base_safety=30, radius_m=150):
    count = sum(1 for _, row in df_entertain.iterrows() if geodesic((lat, lon), (row['latitude'], row['longitude'])).meters <= radius_m)
    safety = base_safety - count*1.5
    return max(safety, 0)

# -----------------------
# 색상 범위
# -----------------------
def get_color(percent):
    if percent <= 30:
        return '#00b050'  # 초록
    elif percent <= 50:
        return '#ffff00'  # 노랑
    elif percent <= 80:
        return '#ff8000'  # 주황
    else:
        return '#ff0000'  # 빨강

# -----------------------
# SVG 별 생성 (카테고리와 퍼센트 함께 표시)
# -----------------------
def create_star_svg(percent, category):
    color = get_color(percent)
    fill_height = 24 * percent / 100
    svg = f"""
    <svg width="120" height="120" viewBox="0 0 24 24">
      <polygon points="12,2 15,10 24,10 17,15 19,24 12,19 5,24 7,15 0,10 9,10"
        fill="white" stroke="yellow" stroke-width="1"/>
      <clipPath id="clip">
        <rect x="0" y="{24-fill_height}" width="24" height="{fill_height}"/>
      </clipPath>
      <polygon points="12,2 15,10 24,10 17,15 19,24 12,19 5,24 7,15 0,10 9,10"
        fill="{color}" stroke="none" clip-path="url(#clip)"/>
      <text x="12" y="14" font-size="4" font-weight="bold" text-anchor="middle" fill="black">{category}</text>
      <text x="12" y="20" font-size="4" font-weight="bold" text-anchor="middle" fill="black">{int(percent)}%</text>
    </svg>
    """
    return svg

# -----------------------
# 마커 추가 함수
# -----------------------
def add_marker(df, category):
    for _, row in df.iterrows():
        if category=="유흥업소":
            percent = compute_entertain_risk(row['latitude'], row['longitude'], df_entertain)
        elif category=="보호구역":
            percent = compute_protect_safety(row['latitude'], row['longitude'], df_entertain)
        else:  # 상가
            percent = row['risk']
        folium.Marker(
            location=[row['latitude'], row['longitude']],
            icon=folium.DivIcon(
                html=f"<div style='text-align:center'>{create_star_svg(percent, category)}</div>"
            ),
            popup=f"{row['name']} ({int(percent)}%)"
        ).add_to(m)

# -----------------------
# 모든 마커 추가
# -----------------------
add_marker(df_entertain, "유흥업소")
add_marker(df_protect, "보호구역")
add_marker(df_shop, "상가")

# -----------------------
# 범례 추가
# -----------------------
template = """
{% macro html(this, kwargs) %}
<div style="
    position: fixed; 
    bottom: 50px; left: 50px; width: 150px; height: 150px; 
    z-index:9999; font-size:14px;
    background-color:white;
    border:2px solid grey;
    padding: 10px;
    ">
<b>위험도 범위</b><br>
<i style="background:#00b050;width:12px;height:12px;display:inline-block;margin-right:5px"></i>0~30%<br>
<i style="background:#ffff00;width:12px;height:12px;display:inline-block;margin-right:5px"></i>31~50%<br>
<i style="background:#ff8000;width:12px;height:12px;display:inline-block;margin-right:5px"></i>51~80%<br>
<i style="background:#ff0000;width:12px;height:12px;display:inline-block;margin-right:5px"></i>81~100%<br>
</div>
{% endmacro %}
"""
macro = MacroElement()
macro._template = Template(template)
m.get_root().add_child(macro)

# -----------------------
# 지도 저장
# -----------------------
output_path = r"C:\Users\PC_1M\Desktop\junggu_map_star_inside.html"
m.save(output_path)
print(f"중구 위험도 지도 생성 완료: {output_path}")
