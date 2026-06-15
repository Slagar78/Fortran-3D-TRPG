# mapeditor.rb – Super Simple 3D Editor (color = block type)
require 'raylib'
require 'fileutils'

shared_lib_path = Gem::Specification.find_by_name('raylib-bindings').full_gem_path + '/lib/'
case RUBY_PLATFORM
when /mswin|msys|mingw|cygwin/
  Raylib.load_lib(shared_lib_path + 'libraylib.dll')
end
include Raylib

# ---------- constants ----------
GRID_SIZE = 16
CELL_SIZE = 1.0
CELL_HEIGHT = 1.0
MAX_HEIGHT = 8

# Цвета и соответствующие им типы (серый = пол, остальные = стена)
# Для кодирования: тип*8 + цвет
# тип: 1=пол (проходимый), 2=стена (непроходимая)
COLOR_NAMES = {
  0 => "Пол (серый)",
  1 => "Стена (красный)",
  2 => "Дерево (зелёный)",
  3 => "Вода (голубой)"
}

COLORS_PALETTE = [
  GRAY,   # 0 – пол
  RED,    # 1 – стена
  GREEN,  # 2 – дерево
  BLUE,   # 3 – вода (используем BLUE как голубой)
]

def block_type_for_color(color_idx)
  color_idx == 0 ? 1 : 2   # 1=FLOOR, 2=WALL
end

def encode_layer(color_idx)
  type = block_type_for_color(color_idx)
  type * 8 + color_idx
end

def decode_layer(value)
  type = value / 8
  color_idx = value % 8
  [type, color_idx]
end

@current_color_index = 1   # по умолчанию красная стена
@map_data = Array.new(GRID_SIZE) { Array.new(GRID_SIZE) { [] } }
@erase_mode = false

InitWindow(1200, 800, "Simple 3D Map Editor")
SetTargetFPS(60)

# ---------- camera ----------
target_center = Vector3.create(GRID_SIZE/2.0, 0.0, GRID_SIZE/2.0)
distance = 15.0
angle_h = -45.0 * DEG2RAD
angle_v = 30.0 * DEG2RAD
min_distance = 5.0
max_distance = 50.0
min_angle_v = 5.0 * DEG2RAD
max_angle_v = 85.0 * DEG2RAD

camera = Camera.new
camera.up.set(0.0, 1.0, 0.0)
camera.fovy = 45.0
camera.projection = CAMERA_PERSPECTIVE

def update_camera_position(camera, target, distance, angle_h, angle_v)
  camera.position.x = target.x + distance * Math.cos(angle_v) * Math.sin(angle_h)
  camera.position.y = target.y + distance * Math.sin(angle_v)
  camera.position.z = target.z + distance * Math.cos(angle_v) * Math.cos(angle_h)
  camera.target = target
end

update_camera_position(camera, target_center, distance, angle_h, angle_v)

# ---------- palette ----------
PALETTE_X = 10
PALETTE_Y = 680
PALETTE_CELL = 40
PALETTE_GAP = 4
PALETTE_COUNT = COLORS_PALETTE.size

# ---------- buttons ----------
BUTTON_Y = 730
BUTTON_HEIGHT = 40
BUTTON_GAP = 10

def add_button(x, y, w, h, label, action)
  { x: x, y: y, w: w, h: h, label: label, action: action }
end

button_x = 400
btn_save = add_button(button_x, BUTTON_Y, 100, BUTTON_HEIGHT, "Save", :save)
button_x += 100 + BUTTON_GAP
btn_load = add_button(button_x, BUTTON_Y, 100, BUTTON_HEIGHT, "Load", :load)
button_x += 100 + BUTTON_GAP
btn_erase = add_button(button_x, BUTTON_Y, 120, BUTTON_HEIGHT, "Erase (hold)", :erase)
button_x += 120 + BUTTON_GAP
btn_clear = add_button(button_x, BUTTON_Y, 120, BUTTON_HEIGHT, "Clear cell", :clear_cell)

buttons = [btn_save, btn_load, btn_erase, btn_clear]

ADD_INTERVAL = 0.15
REMOVE_INTERVAL = 0.15
add_timer = 0.0
remove_timer = 0.0

state = :edit
save_filename = ""
save_overwrite = nil   # имя файла для перезаписи (если выбрали существующий)
load_files = []
load_selected_index = 0
load_scroll_offset = 0
MAX_VISIBLE_FILES = 10
MAPS_DIR = File.expand_path(File.join(File.dirname(__FILE__), '..', 'data', 'maps'))

# ---------- file helpers ----------
def ensure_maps_dir
  FileUtils.mkdir_p(MAPS_DIR) unless Dir.exist?(MAPS_DIR)
end

def refresh_load_list
  ensure_maps_dir
  @load_files = Dir.glob("#{MAPS_DIR}/*.map").map { |f| File.basename(f, ".map") }
  @load_files.sort!
  @load_selected_index = 0
  @load_scroll_offset = 0
end

def save_map_to_file(filename)
  ensure_maps_dir
  path = "#{MAPS_DIR}/#{filename}.map"
  File.open(path, "w") do |f|
    GRID_SIZE.times do |z|
      line = []
      GRID_SIZE.times do |x|
        stack = @map_data[z][x]
        line << (stack.empty? ? "0" : stack.join(","))
      end
      f.puts line.join(" ")
    end
  end
  puts "Saved map to #{path}"
end

def load_map_from_file(filename)
  path = "#{MAPS_DIR}/#{filename}.map"
  return unless File.exist?(path)
  lines = File.readlines(path).map(&:strip)
  lines.each_with_index do |line, z|
    next if z >= GRID_SIZE
    cells = line.split
    cells.each_with_index do |cell_str, x|
      next if x >= GRID_SIZE
      if cell_str == "0" || cell_str.empty?
        @map_data[z][x] = []
      else
        @map_data[z][x] = cell_str.split(",").map(&:to_i)
      end
    end
  end
  puts "Loaded map from #{path}"
end

def button_clicked?(btn, mouse_x, mouse_y)
  mouse_x >= btn[:x] && mouse_x <= btn[:x] + btn[:w] &&
  mouse_y >= btn[:y] && mouse_y <= btn[:y] + btn[:h]
end

# ---------- main loop ----------
until WindowShouldClose()
  dt = GetFrameTime()

  # camera
  if IsMouseButtonDown(MOUSE_BUTTON_RIGHT)
    delta = GetMouseDelta()
    angle_h -= delta.x * 0.005
    angle_v += delta.y * 0.005
    angle_v = angle_v.clamp(min_angle_v, max_angle_v)
  end

  wheel = GetMouseWheelMove()
  if wheel != 0
    distance -= wheel * 0.5
    distance = distance.clamp(min_distance, max_distance)
  end
  update_camera_position(camera, target_center, distance, angle_h, angle_v)

  # mouse ray
  ray = GetScreenToWorldRay(GetMousePosition(), camera)
  hit_point = nil
  if ray.direction.y != 0
    t = -ray.position.y / ray.direction.y
    if t > 0
      hit_point = Vector3.create(ray.position.x + ray.direction.x * t,
                                 0.0,
                                 ray.position.z + ray.direction.z * t)
    end
  end

  highlighted_cell = nil
  if hit_point
    ix = (hit_point.x / CELL_SIZE).floor
    iz = (hit_point.z / CELL_SIZE).floor
    if ix >= 0 && ix < GRID_SIZE && iz >= 0 && iz < GRID_SIZE
      highlighted_cell = [ix, iz]
    end
  end

  mouse_pos = GetMousePosition()
  mx = mouse_pos.x
  my = mouse_pos.y

  # input
  case state
  when :edit
    if IsMouseButtonPressed(MOUSE_BUTTON_LEFT)
      # palette selection
      PALETTE_COUNT.times do |i|
        px = PALETTE_X + i * (PALETTE_CELL + PALETTE_GAP)
        py = PALETTE_Y
        if mx >= px && mx <= px + PALETTE_CELL && my >= py && my <= py + PALETTE_CELL
          @current_color_index = i
          break
        end
      end

      # buttons
      buttons.each do |btn|
        if button_clicked?(btn, mx, my)
          case btn[:action]
          when :save
            refresh_load_list
            @save_filename = ""
            @save_overwrite = nil
            state = :save_dialog
          when :load
            refresh_load_list
            state = :load_dialog
          when :erase
            @erase_mode = !@erase_mode
          when :clear_cell
            if highlighted_cell
              ix, iz = highlighted_cell
              @map_data[iz][ix].clear
            end
          end
          break
        end
      end
    end

    # Delete / Backspace remove top block
    if highlighted_cell && (IsKeyPressed(KEY_DELETE) || IsKeyPressed(KEY_BACKSPACE))
      ix, iz = highlighted_cell
      stack = @map_data[iz][ix]
      stack.pop if stack.any?
    end

    # continuous draw / erase
    if IsMouseButtonDown(MOUSE_BUTTON_LEFT) && highlighted_cell
      over_ui = false
      # palette
      PALETTE_COUNT.times do |i|
        px = PALETTE_X + i * (PALETTE_CELL + PALETTE_GAP)
        py = PALETTE_Y
        if mx >= px && mx <= px + PALETTE_CELL && my >= py && my <= py + PALETTE_CELL
          over_ui = true
          break
        end
      end
      # buttons
      buttons.each do |btn|
        if button_clicked?(btn, mx, my)
          over_ui = true
          break
        end
      end

      unless over_ui
        ix, iz = highlighted_cell
        stack = @map_data[iz][ix]

        if @erase_mode
          remove_timer -= dt
          if remove_timer <= 0.0 && stack.any?
            stack.pop
            remove_timer = REMOVE_INTERVAL
          end
        else
          add_timer -= dt
          if add_timer <= 0.0 && stack.size < MAX_HEIGHT
            stack.push(encode_layer(@current_color_index))
            add_timer = ADD_INTERVAL
          end
        end
      end
    else
      add_timer = 0.0 unless IsMouseButtonDown(MOUSE_BUTTON_LEFT)
      remove_timer = 0.0 unless IsMouseButtonDown(MOUSE_BUTTON_LEFT)
    end

  when :save_dialog
    # Обработка ввода текста
    key = GetCharPressed()
    while key > 0
      if key >= 32 && key <= 125 && @save_filename.length < 20
        @save_filename += key.chr
      end
      key = GetCharPressed()
    end

    if IsKeyPressed(KEY_BACKSPACE) && @save_filename.length > 0
      @save_filename.chop!
    end

    # Выбор существующего файла стрелками
    if IsKeyPressed(KEY_UP)
      @load_selected_index -= 1 if @load_selected_index > 0
      @load_scroll_offset = @load_selected_index if @load_selected_index < @load_scroll_offset
    elsif IsKeyPressed(KEY_DOWN)
      @load_selected_index += 1 if @load_selected_index < @load_files.size - 1
      if @load_selected_index >= @load_scroll_offset + MAX_VISIBLE_FILES
        @load_scroll_offset = @load_selected_index - MAX_VISIBLE_FILES + 1
      end
    end

    # Enter: если выделен файл – перезаписать, иначе сохранить новое имя
    if IsKeyPressed(KEY_ENTER)
      if @load_files.any? && @load_selected_index < @load_files.size
        save_map_to_file(@load_files[@load_selected_index])
      elsif @save_filename.length > 0
        save_map_to_file(@save_filename)
      end
      state = :edit
    elsif IsKeyPressed(KEY_ESCAPE)
      state = :edit
    end

  when :load_dialog
    if IsKeyPressed(KEY_UP)
      @load_selected_index -= 1 if @load_selected_index > 0
      @load_scroll_offset = @load_selected_index if @load_selected_index < @load_scroll_offset
    elsif IsKeyPressed(KEY_DOWN)
      @load_selected_index += 1 if @load_selected_index < @load_files.size - 1
      if @load_selected_index >= @load_scroll_offset + MAX_VISIBLE_FILES
        @load_scroll_offset = @load_selected_index - MAX_VISIBLE_FILES + 1
      end
    end

    if IsKeyPressed(KEY_ENTER) && @load_files.any?
      load_map_from_file(@load_files[@load_selected_index])
      state = :edit
    elsif IsKeyPressed(KEY_ESCAPE)
      state = :edit
    end
  end

  # drawing
  BeginDrawing()
  ClearBackground(SKYBLUE)

  BeginMode3D(camera)

  DrawPlane(Vector3.create(GRID_SIZE/2.0, -0.01, GRID_SIZE/2.0),
            Vector2.create(GRID_SIZE, GRID_SIZE), LIGHTGRAY)

  (0..GRID_SIZE).each do |i|
    DrawLine3D(Vector3.create(i, 0, 0), Vector3.create(i, 0, GRID_SIZE), DARKGRAY)
    DrawLine3D(Vector3.create(0, 0, i), Vector3.create(GRID_SIZE, 0, i), DARKGRAY)
  end

  GRID_SIZE.times do |iz|
    GRID_SIZE.times do |ix|
      stack = @map_data[iz][ix]
      next if stack.empty?
      center_x = ix * CELL_SIZE + CELL_SIZE/2.0
      center_z = iz * CELL_SIZE + CELL_SIZE/2.0
      stack.each_with_index do |encoded, layer|
        type, color_idx = decode_layer(encoded)
        color = COLORS_PALETTE[color_idx] || GRAY
        y = layer * CELL_HEIGHT + CELL_HEIGHT/2.0
        pos = Vector3.create(center_x, y, center_z)
        DrawCube(pos, CELL_SIZE, CELL_HEIGHT, CELL_SIZE, color)
        DrawCubeWires(pos, CELL_SIZE, CELL_HEIGHT, CELL_SIZE, BLACK)
      end
    end
  end

  if state == :edit && highlighted_cell
    ix, iz = highlighted_cell
    stack = @map_data[iz][ix]
    next_layer = stack.size
    if next_layer < MAX_HEIGHT
      hl_y = next_layer * CELL_HEIGHT + CELL_HEIGHT/2.0
      hl_pos = Vector3.create(ix * CELL_SIZE + CELL_SIZE/2.0, hl_y, iz * CELL_SIZE + CELL_SIZE/2.0)
      if @erase_mode
        DrawCubeWires(hl_pos, CELL_SIZE, CELL_HEIGHT, CELL_SIZE, RED)
      else
        hl_color = Fade(COLORS_PALETTE[@current_color_index] || GRAY, 0.5)
        DrawCube(hl_pos, CELL_SIZE, CELL_HEIGHT, CELL_SIZE, hl_color)
        DrawCubeWires(hl_pos, CELL_SIZE, CELL_HEIGHT, CELL_SIZE, RED)
      end
    end
  end

  EndMode3D()

  # 2D UI
  case state
  when :edit
    DrawText("Mode: #{@erase_mode ? 'ERASE' : 'DRAW'} | Color: #{COLOR_NAMES[@current_color_index]}", 10, 10, 20, BLACK)
    # palette
    PALETTE_COUNT.times do |i|
      x = PALETTE_X + i * (PALETTE_CELL + PALETTE_GAP)
      y = PALETTE_Y
      rect = Rectangle.new; rect.x = x; rect.y = y; rect.width = PALETTE_CELL; rect.height = PALETTE_CELL
      DrawRectangleRec(rect, COLORS_PALETTE[i])
      DrawRectangleLines(x, y, PALETTE_CELL, PALETTE_CELL, BLACK)
      if i == @current_color_index
        DrawRectangleLines(x-2, y-2, PALETTE_CELL+4, PALETTE_CELL+4, RED)
      end
    end

    buttons.each do |btn|
      rect = Rectangle.new; rect.x = btn[:x]; rect.y = btn[:y]; rect.width = btn[:w]; rect.height = btn[:h]
      DrawRectangleRec(rect, LIGHTGRAY)
      DrawRectangleLines(btn[:x], btn[:y], btn[:w], btn[:h], BLACK)
      text_width = MeasureText(btn[:label], 20)
      text_x = btn[:x] + (btn[:w] - text_width) / 2
      text_y = btn[:y] + (btn[:h] - 20) / 2
      DrawText(btn[:label], text_x, text_y, 20, BLACK)
    end

    if @erase_mode
      btn = buttons.find { |b| b[:action] == :erase }
      if btn
        DrawRectangleLines(btn[:x]-2, btn[:y]-2, btn[:w]+4, btn[:h]+4, RED)
      end
    end

  when :save_dialog
    DrawRectangle(0, 0, 1200, 800, Fade(BLACK, 0.5))
    DrawText("Save map to data/maps/", 300, 100, 30, WHITE)
    # Список существующих карт
    if @load_files.any?
      DrawText("Existing maps (select to overwrite):", 300, 150, 20, LIGHTGRAY)
      visible = @load_files[@load_scroll_offset, MAX_VISIBLE_FILES] || []
      visible.each_with_index do |name, i|
        y = 180 + i * 30
        if @load_scroll_offset + i == @load_selected_index
          DrawText("> #{name}", 300, y, 25, YELLOW)
        else
          DrawText("  #{name}", 300, y, 25, LIGHTGRAY)
        end
      end
    end
    DrawText("Or type new name:", 300, 440, 20, LIGHTGRAY)
    DrawText("Name: #{@save_filename}_", 300, 470, 30, YELLOW)
    DrawText("Enter to save, Esc to cancel", 300, 520, 20, LIGHTGRAY)

  when :load_dialog
    DrawRectangle(0, 0, 1200, 800, Fade(BLACK, 0.5))
    DrawText("Load map from data/maps/", 300, 150, 30, WHITE)
    if @load_files.empty?
      DrawText("No maps found...", 300, 200, 25, LIGHTGRAY)
    else
      visible = @load_files[@load_scroll_offset, MAX_VISIBLE_FILES] || []
      visible.each_with_index do |name, i|
        y = 200 + i * 30
        if @load_scroll_offset + i == @load_selected_index
          DrawText("> #{name}", 300, y, 25, YELLOW)
        else
          DrawText("  #{name}", 300, y, 25, LIGHTGRAY)
        end
      end
      DrawText("Arrows: select  Enter: load  Esc: cancel", 300, 500, 20, LIGHTGRAY)
    end
  end

  DrawFPS(1100, 10)
  EndDrawing()
end

CloseWindow()